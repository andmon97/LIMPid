# MonteLI
A simple language interpreter of the IMP Language, written in Haskell, for didactic purposes. 

This work is part of the *"Formal Methods in Computer Science"* exam at *University of Bari "Aldo Moro".*

## Features
MonteLI is an interpreter (with a parser) of an IMP language composed of some constructs:
- **skip** performs a jump to the next instruction;
- **assignment** assigns a value to a variable by calculating it if it is written in the form of an expression;
- **If-then-else** conditional control structure;
- **Do-while** iterative control structure of the do-while type
- **While-do** iterative control structure of the while-do type;
- **For** iterative control structure of the for type;

*MonteLI* uses dynamic typization, accepts integer and Boolean data.

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
