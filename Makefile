REBAR = /usr/local/lib/erlang/bin/escript rebar
INSTALL = /usr/bin/install -c
SED = /usr/bin/sed
ERL = /usr/local/bin/erl

prefix = /usr/local/ejaberd1407
exec_prefix = ${prefix}

DESTDIR =

# /etc/ejabberd/
ETCDIR = $(DESTDIR)${prefix}/etc/ejabberd

# /sbin/
SBINDIR = $(DESTDIR)${exec_prefix}/sbin

# /lib/ejabberd/
EJABBERDDIR = $(DESTDIR)${exec_prefix}/lib/ejabberd

# /share/doc/ejabberd
PACKAGE_TARNAME = ejabberd
datarootdir = ${prefix}/share
DOCDIR = $(DESTDIR)${datarootdir}/doc/${PACKAGE_TARNAME}

# /usr/lib/ejabberd/ebin/
BEAMDIR = $(EJABBERDDIR)/ebin

# /usr/lib/ejabberd/include/
INCLUDEDIR = $(EJABBERDDIR)/include

# /usr/lib/ejabberd/priv/
PRIVDIR = $(EJABBERDDIR)/priv

# /usr/lib/ejabberd/priv/bin
PBINDIR = $(PRIVDIR)/bin

# /usr/lib/ejabberd/priv/lib
SODIR = $(PRIVDIR)/lib

# /usr/lib/ejabberd/priv/msgs
MSGSDIR = $(PRIVDIR)/msgs

# /var/lib/ejabberd/
SPOOLDIR = $(DESTDIR)${prefix}/var/lib/ejabberd

# /var/lock/ejabberdctl
CTLLOCKDIR = $(DESTDIR)${prefix}/var/lock/ejabberdctl

# /var/lib/ejabberd/.erlang.cookie
COOKIEFILE = $(SPOOLDIR)/.erlang.cookie

# /var/log/ejabberd/
LOGDIR = $(DESTDIR)${prefix}/var/log/ejabberd

INSTALLUSER=
# if no user was enabled, don't set privileges or ownership
ifeq ($(INSTALLUSER),)
  O_USER=
  G_USER=
  CHOWN_COMMAND=echo
  CHOWN_OUTPUT=/dev/null
  INIT_USER=root
else
  O_USER=-o $(INSTALLUSER)
  G_USER=-g $(INSTALLUSER)
  CHOWN_COMMAND=chown
  CHOWN_OUTPUT=&1
  INIT_USER=$(INSTALLUSER)
endif

all: deps src

deps: deps/.got

deps/.got:
	rm -rf deps/.got
	rm -rf deps/.built
	$(REBAR) get-deps && :> deps/.got

deps/.built: deps/.got
	$(REBAR) compile && :> deps/.built

src: deps/.built
	$(REBAR) skip_deps=true compile

update:
	rm -rf deps/.got
	rm -rf deps/.built
	$(REBAR) update-deps && :> deps/.got

translations:
	contrib/extract_translations/prepare-translation.sh -updateall

doc:
	echo making $$target in doc; \
	(cd doc && $(MAKE) $$target) || exit 1

edoc:
	$(ERL) -noinput +B -eval \
        'case edoc:application(ejabberd, ".", []) of ok -> halt(0); error -> halt(1) end.'

spec:
	$(ERL) -noinput +B -pa ebin -pa deps/*/ebin -eval \
	'case xml_gen:compile("tools/xmpp_codec.spec") of ok -> halt(0); _ -> halt(1) end.'

DLLs := $(wildcard deps/*/priv/*.so) $(wildcard deps/*/priv/lib/*.so)

install: all
	#
	# Configuration files
	$(INSTALL) -d -m 750 $(G_USER) $(ETCDIR)
	[ -f $(ETCDIR)/ejabberd.yml ] \
		&& $(INSTALL) -b -m 640 $(G_USER) ejabberd.yml.example $(ETCDIR)/ejabberd.yml-new \
		|| $(INSTALL) -b -m 640 $(G_USER) ejabberd.yml.example $(ETCDIR)/ejabberd.yml
	$(SED) -e "s*{{rootdir}}*/usr/local/ejaberd1407*" \
		-e "s*{{installuser}}**" \
		-e "s*{{libdir}}*${exec_prefix}/lib*" \
		-e "s*{{sysconfdir}}*${prefix}/etc*" \
		-e "s*{{localstatedir}}*${prefix}/var*" \
		-e "s*{{docdir}}*${datarootdir}/doc/${PACKAGE_TARNAME}*" \
		-e "s*{{erl}}*/usr/local/bin/erl*" ejabberdctl.template \
		> ejabberdctl.example
	[ -f $(ETCDIR)/ejabberdctl.cfg ] \
		&& $(INSTALL) -b -m 640 $(G_USER) ejabberdctl.cfg.example $(ETCDIR)/ejabberdctl.cfg-new \
		|| $(INSTALL) -b -m 640 $(G_USER) ejabberdctl.cfg.example $(ETCDIR)/ejabberdctl.cfg
	$(INSTALL) -b -m 644 $(G_USER) inetrc $(ETCDIR)/inetrc
	#
	# Administration script
	[ -d $(SBINDIR) ] || $(INSTALL) -d -m 755 $(SBINDIR)
	$(INSTALL) -m 550 $(G_USER) ejabberdctl.example $(SBINDIR)/ejabberdctl
	#
	# Init script
	$(SED) -e "s*@ctlscriptpath@*$(SBINDIR)*" \
		-e "s*@installuser@*$(INIT_USER)*" ejabberd.init.template \
		> ejabberd.init
	chmod 755 ejabberd.init
	#
	# Binary Erlang files
	$(INSTALL) -d $(BEAMDIR)
	$(INSTALL) -m 644 ebin/*.app $(BEAMDIR)
	$(INSTALL) -m 644 ebin/*.beam $(BEAMDIR)
	$(INSTALL) -m 644 deps/*/ebin/*.app $(BEAMDIR)
	$(INSTALL) -m 644 deps/*/ebin/*.beam $(BEAMDIR)
	#rm -f $(BEAMDIR)/configure.beam
	#
	# ejabberd header files
	$(INSTALL) -d $(INCLUDEDIR)
	$(INSTALL) -m 644 include/*.hrl $(INCLUDEDIR)
	$(INSTALL) -m 644 deps/*/include/*.hrl $(INCLUDEDIR)
	#
	# Binary C programs
	$(INSTALL) -d $(PBINDIR)
	$(INSTALL) -m 750 $(O_USER) tools/captcha.sh $(PBINDIR)
	-[ -f deps/p1_pam/priv/bin/epam ] \
		&& $(INSTALL) -m 750 $(O_USER) deps/p1_pam/priv/bin/epam $(PBINDIR)
	#
	# Binary system libraries
	$(INSTALL) -d $(SODIR)
	$(INSTALL) -m 644 $(DLLs) $(SODIR)
	#
	# Translated strings
	$(INSTALL) -d $(MSGSDIR)
	$(INSTALL) -m 644 priv/msgs/*.msg $(MSGSDIR)
	#
	# Spool directory
	$(INSTALL) -d -m 750 $(O_USER) $(SPOOLDIR)
	$(CHOWN_COMMAND) -R  $(SPOOLDIR) >$(CHOWN_OUTPUT)
	chmod -R 750 $(SPOOLDIR)
	[ ! -f $(COOKIEFILE) ] || { $(CHOWN_COMMAND)  $(COOKIEFILE) >$(CHOWN_OUTPUT) ; chmod 400 $(COOKIEFILE) ; }
	#
	# ejabberdctl lock directory
	$(INSTALL) -d -m 750 $(O_USER) $(CTLLOCKDIR)
	$(CHOWN_COMMAND) -R  $(CTLLOCKDIR) >$(CHOWN_OUTPUT)
	chmod -R 750 $(CTLLOCKDIR)
	#
	# Log directory
	$(INSTALL) -d -m 750 $(O_USER) $(LOGDIR)
	$(CHOWN_COMMAND) -R  $(LOGDIR) >$(CHOWN_OUTPUT)
	chmod -R 750 $(LOGDIR)
	#
	# Documentation
	$(INSTALL) -d $(DOCDIR)
	$(INSTALL) -m 644 doc/dev.html $(DOCDIR)
	$(INSTALL) -m 644 doc/guide.html $(DOCDIR)
	$(INSTALL) -m 644 doc/*.png $(DOCDIR)
	[ -f doc/guide.pdf ] \
		&& $(INSTALL) -m 644 doc/guide.pdf $(DOCDIR) \
		|| echo "No doc/guide.pdf was built"
	$(INSTALL) -m 644 COPYING $(DOCDIR)

uninstall: uninstall-binary

uninstall-binary:
	rm -f  $(SBINDIR)/ejabberdctl
	rm -fr $(DOCDIR)
	rm -f  $(BEAMDIR)/*.beam
	rm -f  $(BEAMDIR)/*.app
	rm -fr $(BEAMDIR)
	rm -f  $(INCLUDEDIR)/*.hrl
	rm -fr $(INCLUDEDIR)
	rm -fr $(PBINDIR)
	rm -f  $(SODIR)/*.so
	rm -fr $(SODIR)
	rm -f  $(MSGSDIR)/*.msgs
	rm -fr $(MSGSDIR)
	rm -fr $(PRIVDIR)
	rm -fr $(EJABBERDDIR)

uninstall-all: uninstall-binary
	rm -rf $(ETCDIR)
	rm -rf $(EJABBERDDIR)
	rm -rf $(SPOOLDIR)
	rm -rf $(CTLLOCKDIR)
	rm -rf $(LOGDIR)

clean:
	rm -rf deps/.got
	rm -rf deps/.built
	rm -rf test/*.beam
	$(REBAR) clean

clean-rel:
	rm -rf rel/ejabberd

distclean: clean clean-rel
	rm -f config.status
	rm -f config.log
	rm -rf autom4te.cache
	rm -rf deps
	rm -rf ebin
	rm -f Makefile
	rm -f vars.config
	rm -f src/ejabberd.app.src
	[ ! -f ../ChangeLog ] || rm -f ../ChangeLog

rel: all
	$(REBAR) generate

TAGS:
	etags *.erl

Makefile: Makefile.in

deps := $(wildcard deps/*/ebin)

dialyzer/erlang.plt:
	@mkdir -p dialyzer
	@dialyzer --build_plt --output_plt dialyzer/erlang.plt \
	-o dialyzer/erlang.log --apps kernel stdlib sasl crypto \
	public_key ssl mnesia inets odbc tools compiler erts webtool \
	runtime_tools asn1 observer xmerl et gs wx syntax_tools; \
	status=$$? ; if [ $$status -ne 2 ]; then exit $$status; else exit 0; fi

dialyzer/deps.plt:
	@mkdir -p dialyzer
	@dialyzer --build_plt --output_plt dialyzer/deps.plt \
	-o dialyzer/deps.log $(deps); \
	status=$$? ; if [ $$status -ne 2 ]; then exit $$status; else exit 0; fi

dialyzer/ejabberd.plt:
	@mkdir -p dialyzer
	@dialyzer --build_plt --output_plt dialyzer/ejabberd.plt \
	-o dialyzer/ejabberd.log ebin; \
	status=$$? ; if [ $$status -ne 2 ]; then exit $$status; else exit 0; fi

erlang_plt: dialyzer/erlang.plt
	@dialyzer --plt dialyzer/erlang.plt --check_plt -o dialyzer/erlang.log; \
	status=$$? ; if [ $$status -ne 2 ]; then exit $$status; else exit 0; fi

deps_plt: dialyzer/deps.plt
	@dialyzer --plt dialyzer/deps.plt --check_plt -o dialyzer/deps.log; \
	status=$$? ; if [ $$status -ne 2 ]; then exit $$status; else exit 0; fi

ejabberd_plt: dialyzer/ejabberd.plt
	@dialyzer --plt dialyzer/ejabberd.plt --check_plt -o dialyzer/ejabberd.log; \
	status=$$? ; if [ $$status -ne 2 ]; then exit $$status; else exit 0; fi

dialyzer: erlang_plt deps_plt ejabberd_plt
	@dialyzer --plts dialyzer/*.plt --no_check_plt \
	--get_warnings -o dialyzer/error.log ebin; \
	status=$$? ; if [ $$status -ne 2 ]; then exit $$status; else exit 0; fi

test:
	@echo "************************** NOTICE ***************************************"
	@cat test/README
	@echo "*************************************************************************"
	$(REBAR) skip_deps=true ct

.PHONY: src doc edoc dialyzer Makefile TAGS clean clean-rel distclean rel \
	install uninstall uninstall-binary uninstall-all translations deps test spec \
	erlang_plt deps_plt ejabberd_plt
