APPLICATIONS = zincBB mochiweb nitrogen

all: 
	erl -make
	
clean:
	@(cd lib/zincBB; rm -Rf ./ebin/*.beam)
	@rm -Rf ./wwwroot/nitrogen

distclean:
	@for i in $(APPLICATIONS); do \
	(cd lib/$$i; rm -Rf ./ebin/*.beam); done
	@rm -Rf ./wwwroot/nitrogen
	
