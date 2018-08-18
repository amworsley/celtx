BDIR := mozilla
IDIR := objdir/celtx/installer
DESTDIR := /usr/local
all: $(BDIR)/mozconfig-nodebug-linux $(BDIR)/.mozconfig
	cd $(BDIR); make -f client.mk build
	cd $(IDIR); make

$(BDIR)/mozconfig-nodebug-linux:
	sed 's/%LOCALE%/en-US/' \
	  $(BDIR)/mozconfig-nodebug-linux.in > $(BDIR)/mozconfig-nodebug-linux

$(BDIR)/.mozconfig:
	ln -sf mozconfig-nodebug-linux $(BDIR)/.mozconfig

VER := 0.1.1

PKGNAME := open-celtx
TFILE := $(PKGNAME)_$(VER).tar

archive: $(TFILE)

$(TFILE):
	git archive -o $(TFILE) --prefix $(PKGNAME)-$(VER)/ HEAD
	git tag v$(VER) HEAD

install:
	install -d $(DESTDIR)/usr/celtx
	cp -a objdir/dist/celtx/. $(DESTDIR)/usr/celtx
