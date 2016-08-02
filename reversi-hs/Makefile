TARGETS=reversi-serv
TARGETC=reversi
SRCS=Play.hs Command.hs
FLAGS=-XFlexibleContexts

all: $(TARGETS) $(TARGETC)

$(TARGETS): Server.hs $(SRCS)
	ghc -O --make -o $(TARGETS) Server.hs  $(FLAGS)

$(TARGETC): Client.hs $(SRCS)
	ghc -O --make -o $(TARGETC) Client.hs  $(FLAGS)


clean: 
	rm -f *.o
	rm -f *.hi
	rm -f $(TARGETS)
	rm -f $(TARGETS).exe
	rm -f $(TARGETC)
	rm -f $(TARGETC).exe
