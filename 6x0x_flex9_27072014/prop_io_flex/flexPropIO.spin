{{                    propIO   Interface to N8VEM and propeller

                               V 0.91

          By:   David Mehaffy (yoda)         

     Credits:

      Andrew Lynch (lynchaj)          for creating the N8VEM
      Vince Briel  (vbriel)           for the PockeTerm which a lot of code is shared here
      Jeff Ledger  (oldbitcollector)  for base terminal code
      Ray Rodrick  (cluso99)          for the TriBladeProp that shares some of these ideas
                                      for using the CPM to SD

      Updated by Borut Korosin for CPM/FLEX usage
      adapted for different geometries in flex
      Corrected bugs in port implementation
}}

CON
  _clkmode = xtal1 + pll16x
  _xinfreq = 5_000_000
  
  video_base = 16                                   'Start pin for VGA

'' Terminal Colors
   TURQUOISE         = $29
   BLUE              = $27
   BABYBLUE          = $95
   RED               = $C1
   GREEN             = $99
   GOLDBROWN         = $A2
   AMBERDARK         = $E2
   LAVENDER          = $A5
   WHITE             = $FF
   HOTPINK           = $C9
   GOLD              = $D9
   PINK              = $C5

   spiDO             = 24
   spiClk            = 25
   spiDI             = 26
   spiCS             = 27

   DSKREAD           = 1
   DSKWRITE          = 2
   DSKREQUEST        = $EE
   DSKIODONE         = $AA
   DSKRESTART        = $04
   DSKIORDY          = $10

'#define DEBUG_SD
'#define DOS65
#define FLEX
'#define CPM

   MAX_SECTOR = 32
   SD_SECTOR_SIZE = 512
#ifdef CPM
   SECTOR_SIZE = 128
#endif
#ifdef FLEX
   SECTOR_SIZE = 256
#endif
#ifdef DOS65
   SECTOR_SIZE = 128
#endif


OBJ
  txt  :  "VGA_1024"                                 'VGA Terminal Driver
  port :  "N8VEM_Ports"
  kb   :  "Keyboard"
  sd   :  "fsrw"
  'dbg : "Parallax Serial Terminal"                      ' Serial Port Driver (debug output)
  'dbg : "Parallax Serial Terminal Null"                 ' Do nothing for debug output
  'sio : "FullDuplexSerial"                              ' Serial I/O
  'sio : "FullDuplexSerialNull"                          ' Dummy driver to use when debugging

VAR
  byte disk_buff[520]
  long  x
  long CLR[11]
  byte ch
  long temp
  long status
  long dsk_status
  long dsk_request
  long keyboard
  long drive_base[16]
  long drive_sector[16]
  long drive_track[16]
  long sd_block_number
  long current_block_number
  long block_offset
  long sector_offset
  byte disk_command
  byte drive_number
  long sector_number
  long err
  long i, j,k,index
  long tf

PUB   main
  'dbg.Start(115200)

  CLR[1]:=TURQUOISE
  CLR[2]:=BLUE                  'GREEN
  CLR[3]:=BABYBLUE              'opecna
  CLR[4]:=RED
  CLR[5]:=GREEN                 ' magenta
  CLR[6]:=GOLDBROWN             ' yellow
  CLR[7]:=WHITE                 ' white
  CLR[8]:=HOTPINK               '
  CLR[9]:=GOLD                  ' pink
  CLR[10]:=PINK
  CLR[11]:=AMBERDARK

  status := 0
  
  txt.start(video_base)
  txt.color(CLR[1])

  txt.cls1(9600, 5, 1, 0, 0)
  txt.inv(0)
  txt.cursorset(4)

  kb.start(14,15)
  txt.str(string("starting propIO"))
  crlf
  'DbgStr(string("starting propIO"))
  'DbgCrlf

  '!!! comment for test purposes 6809 n8vem board !!!
  FindSDblock
  txt.str(string(" after findSD "))
  crlf
  'DbgStr(string("after findSD"))
  'DbgCrlf
  port.start
  current_block_number := 0

  j := @disk_buff
  txt.str(string("Disk buffer start = "))
  txt.hex(j,4)
  crlf
  i := 0
  repeat
    disk_command := port.get_disk_command
    if disk_command ==  DSKREQUEST
        port.set_disk_command(0)
        port.set_disk_status(4)

    if disk_command ==  DSKRESTART
        FindSDblock
        port.set_disk_command(0)
        port.set_disk_status($40)

    dsk_status := port.get_disk_status
    if dsk_status == $80
      'txt.str(string("Status = $80"))
      disk_command := port.get_disk_command
      'txt.hex(disk_command,2)
      'crlf
      if disk_command == DSKREAD
        'txt.str(string("READ"))
        'crlf
        port.set_disk_command(0)
        disk_read
      if disk_command == DSKWRITE
        'txt.str(string("WRITE"))
        'crlf
        disk_write
    temp := port.get_status                             'see if we are ready for another key
    if temp <> 1                                        'if 0 then we can send another key if in buffer
      if kb.gotkey                                      'check to see if key in buffer
        keyboard := kb.getkey                           'yes so stuff it in key and
{{        
        txt.statprint(37,25,string("      "))
        txt.statnum(37,25,keyboard)
}}
        if keyboard > 576
          if keyboard < 603
            keyboard := keyboard - 576
        if keyboard > 608 and keyboard < 635
          keyboard := keyboard - 608
        if keyboard == 200
          keyboard := 08
        if keyboard == 203
          keyboard := 27
        
        port.set_key(keyboard) 
        port.set_status(1)                               'set status that we have a key
    ch := port.vga_check                                 'see if N8VEM has output a character
    if ch <> 255
'      txt.hex(ch,2)
      txt.process_char(ch)
      ch := -1
      waitcnt(5_000 + cnt)
    

PRI FindSDblock | r
  txt.str(string(" before mount "))
  crlf
  r := sd.mount_explicit(spiDO, spiClk, spiDI, spiCS)
  txt.str(string(" after mount "))
  crlf
  CheckError(r)
  txt.str(string(" in FindSDblock"))
  crlf
#ifdef CPM
  FindDriveBase(0, string("CPM_0.DSK"))
  FindDriveBase(1, string("CPM_1.DSK"))
  FindDriveBase(2, string("CPM_2.DSK"))
  FindDriveBase(3, string("CPM_3.DSK"))
#endif CPM
#ifdef DOS65
  FindDriveBase(0, string("DOS65_0.DSK"))
  FindDriveBase(1, string("DOS65_1.DSK"))
  FindDriveBase(2, string("DOS65_2.DSK"))
  FindDriveBase(3, string("DOS65_3.DSK"))
  FindDriveBase(15, string("BOOT.SYS"))
#endif DOS65
#ifdef FLEX
  FindDriveBase(0, string("FLEX_0.DSK"))
  FindDriveBase(1, string("FLEX_1.DSK"))
  FindDriveBase(2, string("FLEX_2.DSK"))
  FindDriveBase(3, string("FLEX_3.DSK"))
  FindDriveBase(15, string("FLEX.SYS"))
#endif FLEX

PRI FindDriveBase(n, st) | r                            'initialize disk
  r := sd.popen(st, "r")
  CheckError(r)
  drive_base[n] := sd.getSDdatablock
  txt.str(st)
  txt.str(string(" = "))
  txt.hex(drive_base[n],4)
  drive_sector[n]:= MAX_SECTOR
  drive_track[n]:=$ffffffff                             ' default values

#ifdef FLEX
  port.set_drive_number(n)                               ' FLEX specific!!!
  port.set_track_number(0)                               ' FLEX specific!!!
  port.set_sector_number(2)
  refresh_hd_cache
  drive_track[n]:=disk_buff[$26]
  drive_sector[n]:=disk_buff[$27]
  txt.str(string(", Disk geometry: T:"))
  txt.dec(drive_track[n])
  txt.str(string(" S:"))
  txt.dec(drive_sector[n])
  crlf
#endif FLEX

PRI CheckError(r)
  if r < 0
    txt.str(string("****Error*** $"))
    txt.dec(r)
    txt.str(string("***HALTED***"))
    crlf
    sd.unmount
    repeat

'PRI DbgCrlf
'  dbg.NewLine

'PRI DbgStr(StrPtr)
'  dbg.Str(StrPtr)

'PRI DbgDec(Val)
'  dbg.Dec(Val)

'PRI DbgHex(Val, Digits)
'  dbg.Hex(Val, Digits)

PRI crlf
  txt.process_char(13)
  txt.process_char(10)

PRI disk_read
  refresh_hd_cache
  sector_offset := @disk_buff + block_offset
  port.set_sector_offset(sector_offset)
  port.set_disk_status(DSKIORDY)

PRI disk_write
   refresh_hd_cache
   sector_offset := @disk_buff + block_offset
   {{
   txt.str(string("boff = "))
   txt.hex(block_offset,4)
   txt.str(string("  bnum = "))
   txt.hex(sd_block_number,4)
   txt.str(string("  TK = "))
   txt.hex(port.get_track_number,4)
   txt.str(string("  Sec = "))
   txt.hex(port.get_sector_number,4)
   crlf
   }}
   'write_disk_buf
   'waitcnt(1_000_000)
   port.set_sector_offset(sector_offset)
   port.set_disk_status(DSKIORDY)
   
   dsk_status := port.get_disk_command
   repeat until dsk_status == DSKIODONE
     'txt.str(string("dsk_status = "))
     'txt.hex(dsk_status,4)
     'crlf 
     dsk_status := port.get_disk_command

#ifdef DEBUG_SD
  'txt.str(string("W T:"))
   'txt.hex(port.get_track_number,2)
   'txt.str(string(" S:"))
   'txt.hex(port.get_sector_number,2)
   'crlf

   'write_disk_buf
   'waitcnt(1_000_000)
#endif
  err := \sd.writeSDCard(sd_block_number, @disk_buff, 512)
   CheckError(err)
   port.set_disk_status($20)

PRI write_disk_buf
  index := 0
  repeat  256
    txt.hex(disk_buff[index],2)
    txt.str(string(" "))
    index := index + 1
    if index // 16 == 0
      crlf


PRI refresh_hd_cache | r
'Ensure that the sector number in use is in the current SD block in memory.
'If not load that block to disk buff from SD card.
#ifdef DEBUG_SD
   txt.str(string(",R T:"))
   txt.hex(port.get_track_number,2)
   txt.str(string(" S:"))
   txt.hex(port.get_sector_number,2)
   'crlf
#endif

  sector_number := (port.get_track_number * drive_sector[port.get_drive_number]) + (port.get_sector_number)      'Find correct block for the selected track and sector


  sd_block_number := sector_number >> ((SD_SECTOR_SIZE/SECTOR_SIZE)/2)                 'Determine the SD block of this sector..
                                                                                       '512/128->2, 512/256->1
  sd_block_number += drive_base[port.get_drive_number]  'get the first sector of the selected drive
  block_offset := (sector_number & (SD_SECTOR_SIZE/SECTOR_SIZE-1)) * SECTOR_SIZE           'Determine the position of this sector within the block
                                                                                          '512/128->%11, 512/256->%1
  if sd_block_number <> current_block_number            'If the block is not in disk buffer then load it

    ' this method relies on the SD file using contiguous sectors !!! Packed 4 x 128 bytes per 512 byte sectors
    err := \sd.readSDCard(sd_block_number, @disk_buff, SD_SECTOR_SIZE)     'read the block! (complete 512 bytes)
    CheckError(err)

    current_block_number := sd_block_number           'Make this new block current
   
