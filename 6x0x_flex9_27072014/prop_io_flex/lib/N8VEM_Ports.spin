{{                       FlexHC11 Ports

        31.10.2012 BK corrected bug - missing waitpeq cs_bit, cs_mask on disk_read
        2.11.2012 BK corrected bug if CS is positive polarity


                    +------/WAIT
                    |+-----/RD
                    ||+---- A1
                    |||+--- A0
                    ||||+--/CS
                    |||||
                    |||||
   P15..P0  -->  xxxxxxxx_xxxxxxxx
                          +------+
                          D7....D0


   /RD  A1  A0  CS
     0   0   0   1       Status Port
     0   0   1   1       Keyboard receive
     1   0   1   1       vga out port
     1   1   0   1       Disk command port
     0   1   0   1       Disk command status port
     1   1   1   1       Disk write
     0   1   1   1       Disk read

}}

CON
  DSK_CMD       = 0
  DSK_DRIVE     = 1
  DSK_SECT_NUM  = 2
  DSK_TRK_LO    = 3
  DSK_TRK_HI    = 4
  DSK_DMA_LO    = 5
  DSK_DMA_HI    = 6

Var
  long cog
  long status
  long disk_status
  long key
  long sector_offset
  long dsk_cmdbuf_ptr
  long vga_head
  long vga_tail
  long vga_buffer_ptr

  
  byte vga_buffer[256]
  byte dsk_cmdbuf[10]

PUB start

  stop

  status := 0
  disk_status := 0
  key := 0
  
  longfill(@vga_head, 0, 2)
  vga_buffer_ptr := @vga_buffer
  dsk_cmdbuf_ptr := @dsk_cmdbuf

  cog := cognew(@port_init, @status) + 1


PUB stop
  if cog
    cogstop(cog~ - 1)

PUB set_status(value)
  status := value

PUB get_status : value
  value := status

PUB get_disk_command : value
  value := dsk_cmdbuf[DSK_CMD]

PUB set_disk_command(value)
  dsk_cmdbuf[DSK_CMD] := value
    
PUB set_disk_status(value)
  disk_status := value

PUB get_disk_status : value
  value := disk_status

PUB get_drive_number : value
  value := dsk_cmdbuf[DSK_DRIVE]

PUB set_drive_number(value)
  dsk_cmdbuf[DSK_DRIVE]:=value

PUB get_track_number : value
  value := 256 * dsk_cmdbuf[DSK_TRK_HI] + dsk_cmdbuf[DSK_TRK_LO]

PUB get_sector_number : value
  value := dsk_cmdbuf[DSK_SECT_NUM]

PUB set_track_number(value)
  dsk_cmdbuf[DSK_TRK_HI]:=value >> 8
  dsk_cmdbuf[DSK_TRK_LO]:=value & $ff

PUB set_sector_number(value)
 dsk_cmdbuf[DSK_SECT_NUM]:=  value

PUB set_sector_offset(value)
  sector_offset := value
  
PUB set_key(value)
  key := value

PUB vga_check : vga_byte

'' Checks if there is a character ready (never waits)
'' Returns -1 if not character is ready

  vga_byte := 255
  if vga_tail <> vga_head
    vga_byte := vga_buffer[vga_tail]
    vga_tail := ++vga_tail & $FF

PUB  getc : vga_byte

'' Gets byte from buffer (may wait till there is a byte)
'' returns $00..$FF    

  repeat while (vga_byte := vga_check) < 0

PUB gotchar : available

'' Check if any key in buffer
'' returns t|f

  available := vga_tail <> vga_head


Dat
                        org 0

port_init               mov dira, input_port_mask       'set control + data = inputs, wait as output
                        or  outa, wait_bit              'make sure wait bit is high now
                        andn  outa, test_bit               'Set test bit

                        mov t1,par                      'get parameters
                        mov _status, t1
                        add t1, #4
                        mov dsk_status, t1
                        add t1, #4
                        mov _key,t1
                        add t1, #4
                        mov sector_off, t1
                        add t1, #4
                        rdlong dskcmdptr,t1
                        add t1, #4
                        mov _vga_head, t1
                        add t1, #8                      ' vga_tail is hidden
                        rdlong v_ptr, t1
                        mov cmdcount, #0
                        mov iobytes, #0



port_loop               waitpeq input_read, cs_mask     'Wait for CS to go active
                        andn outa, wait_bit             'toggle wait low
                        mov t1, ina                     'get input bits
                        shr t1, #9                      'get just A0, A1 and /RD
                        and t1, #$07                    'mask just the 3 bits for jump table
                        cmp t1, #1     wz               'see if A0 = 1
              if_z      jmp #keyin                      'A0 = 1, is a keyboard read request
                        cmp t1, #5     wz               'A0 = 1, /RD = 1
              if_z      jmp #vga_out                    'Yes process vga out
                        cmp t1, #6     wz               'A1 = 1, /RD = 1
              if_z      jmp #disk_cmd_in                'Read disk commands
                        cmp t1, #3     wz               'is this a disk read?
              if_z      jmp #disk_read                  'Do disk read
                        cmp t1, #2     wz
              if_z      jmp #disk_cmd_status
                        cmp t1, #7     wz
              if_z      jmp #disk_write
                        
              
                                                        'If got here then it is a status read
status_read             rdlong t1, _status              'get status byte
                        and t1, #$FF                    'make sure byte
                        or  dira, data_bits             'set D0-D7 as ouputs
                        mov outa, t1                    'output the status byte
                        or outa, wait_bit               'release wait
                        waitpeq cs_bit, cs_mask          'wait for CS to go inactive
                        mov dira, input_port_mask       'tri-state data and set cntl for input
                        jmp #port_loop

keyin                   
                        rdlong t1,_key                  'Get Key from Hub
                        and t1, #$FF                    'Make sure it is a byte
                        or dira, data_bits              'set D0-D7 as outputs
                        mov outa, t1                    'output key
                        rdlong t2, _status              'get status
                        mov t2,#0                       'clear key status bit
                        wrlong t2, _status              'write it to the global status
                        or outa, wait_bit               'release wait
                        waitpeq cs_bit, cs_mask          'wait for CS to go inactive
                        mov dira, input_port_mask       'tri-state data and set cntl for input
                        jmp #port_loop      

vga_out                 andn  dira, data_bits           'Make sure D0-D7 (P0 - P7) are set for input
                        mov  c, ina                     'Read data from port
                        or outa, wait_bit               'Release wait bit
                        and  c, #$FF                    'Make sure it is a clean byte
                        rdlong t2, _vga_head            'Should have time to prefetch head here
                        waitpeq  cs_bit, cs_mask        'Now wait to port is unselected

                        add  t2, v_ptr                  'Compute address to store byte
                        wrbyte c, t2                    'Write the byte into vga_buffer
                        sub t2, v_ptr                   'Recompute value of head
                        add t2, #1                      'Increment head
                        and t2, #$FF                    'Make sure it wraps at 255
                        wrlong t2, _vga_head            'Store the new value of vga_head
                        mov dira, input_port_mask       'tri-state data and set cntl for input
                        jmp  #port_loop                 'Go back for more bytes

disk_cmd_in             
                        andn dira, data_bits            'Make sure D0-D7 are set for input
                        mov c, ina                      'Read data from port
                        and c, #$FF                     'Mask it
                        or outa, wait_bit               'Release wait bit
                        waitpeq  cs_bit, cs_mask        'Now wait to port is unselected
                        mov t2, dskcmdptr
                        add t2, cmdcount
                        wrbyte c, t2
                        tjz cmdcount, #check_cmd
                        add cmdcount, #1
                        cmp cmdcount, #7  wz
              if_z      jmp #disk_finish                        
                        jmp #port_loop
disk_finish             mov t2,#$80
                        wrlong  t2, dsk_status
                        mov cmdcount, #0
                        mov iobytes, #0

                        jmp #port_loop
check_cmd               cmp c, #$EE  wz
              if_z      jmp #port_loop
                        cmp c, #$AA  wz
              if_z      jmp #port_loop
                        add cmdcount,#1
                        jmp #port_loop

disk_cmd_status         rdlong t1, dsk_status           'Get Disk Status
                        and t1, #$FF                    'Mask it
                        or dira, data_bits              'Set D0-D7 for output
                        mov outa, t1                    'Output the disk Status
                        or outa, wait_bit               'release the wait bit
                        waitpeq cs_bit, cs_mask          'wait for /CS to go inactive
                        mov dira, input_port_mask       'tri-state data and set cntl for input
                        jmp #port_loop

disk_read               rdlong t2, sector_off           'Get address of byte to read
                        add t2, iobytes                 'add offset in buffer
                        rdbyte c, t2                    'Get byte from buffer
                        add iobytes, #1                 'inc buffer offset
                        and c, #$FF                     'mask byte
                        or dira, data_bits              'set to ouput
                        mov outa, c                     'output the byte to bus
                        or outa, wait_bit               'release wait bit
                        waitpeq cs_bit, cs_mask         'wait for CS to go inactive
                        mov dira, input_port_mask       'tri-state bus again
                        jmp #port_loop

disk_write              andn dira, data_bits            'make sure D0-D7 are set for input
                        rdlong t2, sector_off           'get sector offset to write to
                        add t2, iobytes                 'get increment to write to
                        add iobytes, #1                 'prep for next byte
                        mov c, ina                      'read data
                        or outa, wait_bit
                        and c, #$FF                     'mask it
                        wrbyte c, t2                    'write data into disk buffer
                        waitpeq cs_bit, cs_mask         'wait for CS to go inactive
                        mov dira, input_port_mask       'tri-state data and set cntl for input
                        jmp #port_loop
                        
{{                        Flex11 Ports


                    +------/WAIT
                    |+-----/RD
                    ||+---- A0
                    |||+--- A1
                    ||||+-- CS
                    |||||
                    |||||
   P15..P0  -->  xxxxxxxx_xxxxxxxx
                          +------+
                          D7....D0


   /RD  A1  A0  CS
     0   0   0   1       Status Port
     0   0   1   1       Keyboard receive
     1   0   1   1       vga out port
     1   1   0   1       Disk command port
     0   1   0   1       Disk command status port
     1   1   1   1       Disk write
     0   1   1   1       Disk read

}}


data_bits               LONG  $00FF
input_read              LONG  $0100                     ' cs active high
cs_mask                 LONG  $0100                     ' mask for CS and RD Low ignore A0, A1
cs_bit                  LONG  $0000                     ' cs inactive low
wait_bit                LONG  $1000
input_port_mask         LONG  $3000
test_bit                 LONG  $2000
_status                 res   1
dsk_status              res   1
_key                    res   1
sector_off              res   1
dskcmdptr               res   1
_vga_head               res   1
v_ptr                   res   1
c                       res   1
t1                      res   1
t2                      res   1
cmdcount                res   1
iobytes                 res   1


                        FIT 
