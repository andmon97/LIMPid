# MonteLI
A simple language interpreter of the IMP Language, written in Haskell, for didactic purposes. This work is part of the "Formal Methods in Computer Science" exam at University of Bari "Aldo Moro".

## Grammar
```EBNF
program ::= <command> | <command> <program> 

command ::= <skipcommand> | <assignmentcommand> | <ifcommand> | <whilecommand> | <dowhilecommand> | <forcommand>

skipcommand ::= skip <semicolon> 

assignmentcommand ::= <variable> := (<aexp> | <bexp>) <semicolon> | <letter> := { <array> } <semicolon> 

ifcommand ::= if <space> ( <bexp> ) <space> then { <space> (<program> | <program> else { <space> <program>) } <semicolon>" 

whilecommand ::= while <space> ( <bexp> ) <space> { <space> do <space> <program> <space> } <semicolon>" 

dowhilecommand ::= do <space> { <space> do <space> <program> <space> } while <space> ( <bexp> ) <space> <semicolon>

forcommand ::= for <space> <variable> <space> times <space> { <space> <program> <space> } 

array ::= <afactor> | <afactor> <colon> <array> 

bexp ::= <bterm> | <bterm> <bexpOp> <bexp> 

bterm ::= <bfactor> | ( <bexp> ) | ! <bexp> 

bfactor ::= <aexp> | <aexp> <comparisonOp> <aexp> | <variable> 

aexpr ::= <aterm> | <aterm> <aexpOp1> <aexp> 

aterm ::= <afactor> | <afactor> <aexpOp2> <aterm> 

afactor ::= <apositivefactor> | <anegativefactor> 

anegativefactor ::= - | <apositivefactor> 

apositivefactor ::= <number> | ( <aexp> ) 

number ::= <positivenumber> | <variable> 

positivenumber ::= <digit> | <digit> <positivenumber> 

variable ::= <letter> | <letter> <variabile> | <vet> 

vet ::= <letter>[ <number> ] 

semicolon ::= ; | ; <space> 

colon ::= , | , <space> 

digit ::= 0-9 

aexpOp1 ::= + | - 

aexpOp2 ::= * | / 

bexpOp ::= & | | 

comparisonOp ::= < | > | = | <= | >= | != 

letter ::= a-z 

space ::= " "

```
