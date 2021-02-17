OPTS=-XSOCKET=gnutls -XPRJ_TARGET=UNIX -XTARGET=x86_64-linux-gnu

all:
	gprbuild -g -p $(OPTS) ../aws.gpr
	gprbuild -g -p $(OPTS) prj.gpr

clean:
	rm -fr obj
