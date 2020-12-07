<p align="center" width="100%">
<img src="img/IMPure.jpg" width="350">
</p>

# IMPure
A simple interpreter for the IMP language written in Haskell

# Grammar
```EBNF
program ::=   <command>
          |   <command> <program>

command ::=   <assignment> ";"
          |   <ifThenElse> ";"
          |   <while> ";"
          |   <skip> ";"

assignment ::=    <identifier> "=" <aexp>
            |     <identifier> "=" <bexp>

ifThenElse ::=    "if" (<bexp>) "{" <program> "}"
            |     "if" (<bexp>) "{" <program> "}" "else" "{" <program> "}"

while ::=      "while" (<bexp>) "{" <program> "}"

skip ::=   "skip"

aexp ::=    <aterm>
      |     <aterm> "+" <aexp>
      |     <aterm> "-" <aexp>
      |     <aterm> "*" <aexp>
      |     <aterm> "/" <aexp>

aterm ::=   <positiveterm>
      |     <negativeterm>

negativeterm ::=    "-" <positiveterm>

positiveterm ::=    <positivenumber>
              |     <identifier>

bexp ::=        <truthvalue>
      |         "not" <bexp>
      |         <bexp> "or" <bexp>
      |         <bexp> "and" <bexp>
      |         <aexp> <operator> <aexp>

truthvalue ::=    "True"
            |     "False"
            |     <identifier>

operator ::=    "<"
          |     ">"
          |     "=="
          |     "<="
          |     ">="
          |     "!="

integer ::=   <digit>
          |   <digit> <integer>

digit ::=     [0-9]*

identifier ::=    [a-zA-Z_][a-zA-Z_0-9]*
```
# Execution Example
In this example the IMPure interpreter evaluates the factorial (the code used can be found in the file **test.pure**)
![](img/example.gif)