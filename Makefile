PKGNAME	    = $(shell oasis query name)
PKGVERSION  = $(shell oasis query version)
TARBALL = $(PKGNAME)-$(PKGVERSION).tar.gz

SF_WEB  = /home/groups/o/oc/ocaml-rope/htdocs
SRC_WEB	= web

DISTFILES= rope.ml rope.mli rope_top.ml LICENSE Makefile

.PHONY: all byte native configure doc test install uninstall reinstall

all byte native: configure
	ocaml setup.ml -build

configure: setup.ml
	ocaml $< -configure --enable-tests

setup.ml: _oasis
	oasis setup -setup-update dynamic

test doc install uninstall reinstall: all
	ocaml setup.ml -$@



.PHONY: dist tar
dist tar: $(DISTFILES)
	mkdir $(PKGNAME)-$(PKGVERSION)
	cp --parents -r $(DISTFILES) $(PKGNAME)-$(PKGVERSION)/
#	Generate a setup.ml independent of oasis
	cd $(PKGNAME)-$(PKGVERSION) && oasis setup
	tar -zcvf $(PKG_TARBALL) $(PKGNAME)-$(PKGVERSION)
	$(RM) -rf $(PKGNAME)-$(PKGVERSION)

# Release a Sourceforge tarball and publish the HTML doc
.PHONY: web upload
web-doc: doc
	@ if [ -d doc ] ; then \
	  scp -r doc/ shell.sf.net:$(SF_WEB)/ \
	  && echo "*** Published documentation on SF" ; \
	fi
web:
	@ if [ -d $(SRC_WEB)/ ] ; then \
	  scp $(SRC_WEB)/*.html $(SRC_WEB)/*.css $(SRC_WEB)/*.png LICENSE \
	    shell.sf.net:$(SF_WEB) \
	  && echo "*** Published web site ($(SRC_WEB)/) on SF" ; \
	fi


.PHONY: clean
clean::
	ocaml setup.ml -clean
	$(RM) -f *.cm{i,o,x,a,xa} *.annot *.o *.a *~ META _log $(TARBALL)
	$(RM) $(wildcard bench/*.dat)
