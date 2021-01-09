# cl-prattler

This is a simple Pratt parser for mathematical expressions (easily extended to more complex expression types) based on this implementation of Pratt parsing - [Simple Top-Down
Parsing in Python](https://web.archive.org/web/20201107022724/http://effbot.org/zone/simple-top-down-parsing.htm).

This version generates an AST that can be plugged into a bigger AST scheme for parsing other kinds of entities besides expressions.

## Build

```
CL-USER> (asdf:load-system "cl-prattler")

```

## Run

```
CL-USER> (cl-prattler/main:main)

```

### Sample Run

```
CL-USER> (cl-prattler/main:main)
1
#<(literal 1)>

CL-USER> (cl-prattler/main:main)
-99
#<(neg #<(literal 99)>)>

CL-USER> (cl-prattler/main:main)
+12
#<(pos #<(literal 12)>)>

CL-USER> (cl-prattler/main:main)
1 + +99
#<(add #<(literal 1)> #<(pos #<(literal 99)>)>)>

CL-USER> (cl-prattler/main:main)
11 + -99
#<(add #<(literal 11)> #<(neg #<(literal 99)>)>)>

CL-USER> (cl-prattler/main:main)
(1 + 2) * 3
#<(mul #<(add #<(literal 1)> #<(literal 2)>)> #<(literal 3)>)>

CL-USER> (cl-prattler/main:main)
1 + (2 * 3)
#<(add #<(literal 1)> #<(mul #<(literal 2)> #<(literal 3)>)>)>

CL-USER> (cl-prattler/main:main)
1 + 2 * 3
#<(add #<(literal 1)> #<(mul #<(literal 2)> #<(literal 3)>)>)>

CL-USER> (cl-prattler/main:main)
2 ^ 3 ^ 4
#<(pow #<(literal 2)> #<(pow #<(literal 3)> #<(literal 4)>)>)>

CL-USER> (cl-prattler/main:main)
12 + (2 - -12) + ((11 -2) *    3 + 12 / 2) ^ (11- 9)
#<(add #<(add #<(literal 12)> #<(sub #<(literal 2)> #<(neg #<(literal 12)>)>)>)> #<(pow #<(add #<(mul #<(sub #<(literal 11)> #<(literal 2)>)> #<(literal 3)>)> #<(div #<(literal 12)> #<(literal 2)>)>)> #<(sub #<(literal 11)> #<(literal 9)>)>)>)>

```

## LICENCE

See [LICENCE](LICENCE.md).