CC=./ocamlc.sh
FLAGS=

NAME=exec
TEST=test


ML=display.ml io_handler.ml graph.ml algo.ml 
MLI = $(ML:.ml=.mli)
OBJS = $(ML:.ml=.cmo)
CMI = $(MLI:.mli=.cmi)

all: $(NAME) test

$(NAME): $(OBJS) main.cmo
	$(CC) $(FLAGS) $(OBJS) main.cmo -o $@

$(TEST): $(OBJS) test.cmo
	$(CC) $(FLAGS) $(OBJS) test.cmo -o $@

algo.cmo : algo.ml algo.cmi graph.cmi

%.cmo: %.ml %.cmi
	$(CC) -c $<

%.cmi: %.mli
	$(CC) -c $<

main.cmo: main.ml algo.cmi display.cmi io_handler.cmi
	$(CC) -c main.ml

test.cmo: test.ml algo.cmi display.cmi
	$(CC) -c test.ml

.PHONY: clean

clean:
	rm -f *.cmo *.cmi *.o $(EXEC) $(TEST)
