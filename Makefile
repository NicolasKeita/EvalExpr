##
## EPITECH PROJECT, 2020
## funeval
## File description:
## description
##

NAME = funEvalExpr

all:	$(NAME)
	stack build --copy-bins --local-bin-path .
	mv woot5 $(NAME)

$(NAME):	$(OBJ)

clean:
	stack clean
	rm .stack-work $(NAME).cabal -rf

fclean: clean
	rm -f $(NAME)

re: fclean all

runtest:
	stack test

