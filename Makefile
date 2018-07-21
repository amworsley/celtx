BDIR := mozilla
IDIR := objdir/celtx/installer
all: $(BDIR)/mozconfig-nodebug-linux $(BDIR)/.mozconfig
	cd $(BDIR); make -f client.mk build
	cd $(IDIR); make

$(BDIR)/mozconfig-nodebug-linux:
	sed 's/%LOCALE%/en-US/' \
	  $(BDIR)/mozconfig-nodebug-linux.in > $(BDIR)/mozconfig-nodebug-linux

$(BDIR)/.mozconfig:
	ln -sf $(BDIR)/mozconfig-nodebug-linux $(BDIR)/.mozconfig