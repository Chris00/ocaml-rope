PKGNAME	    = $(shell oasis query name)
PKGVERSION  = $(shell oasis query version)
TARBALL = $(PKGNAME)-$(PKGVERSION).tar.gz

WEB = rope.forge.ocamlcore.org:/home/groups/rope/htdocs
SRC_WEB	= web

DISTFILES= _oasis $(wildcard $(addprefix src/, *.ml *.mli)) LICENSE Makefile \
  $(wildcard $(addprefix bench/, *.ml *.mli))

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
	tar -zcvf $(TARBALL) $(PKGNAME)-$(PKGVERSION)
	$(RM) -rf $(PKGNAME)-$(PKGVERSION)

# Release a Sourceforge tarball and publish the HTML doc
.PHONY: web upload
web-doc: doc
	@ if [ -d API.docdir/ ] ; then \
	  scp -r API.docdir/ $(WEB)/ \
	  && echo "*** Published documentation" ; \
	fi
web:
	@ if [ -d $(SRC_WEB)/ ] ; then \
	  scp $(addprefix $(SRC_WEB)/, *.html *.css *.png) LICENSE $(WEB) \
	  && echo "*** Published web site ($(SRC_WEB)/)" ; \
	fi


.PHONY: clean
clean::
	ocaml setup.ml -clean
	$(RM) -f *.cm{i,o,x,a,xa} *.annot *.o *.a *~ META _log $(TARBALL)
	$(RM) $(wildcard bench/*.dat)
