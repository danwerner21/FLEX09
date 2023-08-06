SPINC=		/home/borutk/apps/propeller/bst19.4/bstc.linux
SPINC_FLAGS=	-Orx -ls -b -e
INCLUDES=	-Llib

VERSION=	0.9.01

all:	propIO

propIO:	propIO.spin  
	$(SPINC) $(SPINC_FLAGS) $(INCLUDES) -o $@ $<
#	minicom
#	gtkterm
	

clean:
	rm -f *.eeprom *.lst *.list *.bck *.o ../lib/*.bck

dist:	clean
	cd .. && \
	rm -f qz80-$(VERSION).zip && \
	zip -r qz80-$(VERSION).zip qz80 lib && \
	zip -d qz80-$(VERSION).zip qz80/CVS\* && \
	zip -d qz80-$(VERSION).zip lib/CVS\*
