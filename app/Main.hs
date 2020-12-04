module Main where

import Grammar
  ( AExp (AVariable, Add, Constant, Mul),
    BExp (Comparison),
    Command (Assignment, VariableDeclaration, While),
    Operator (Le),
    Program (Program),
  )
import Interpreter
  ( emptyState,
    programExec,
  )

main :: IO ()
main = do
  let p =
        Program
          [ VariableDeclaration "i" (Constant 1),
            VariableDeclaration "n" (Constant 5),
            VariableDeclaration "x" (Constant 1),
            While
              (Comparison (AVariable "i") (AVariable "n") Le)
              [ Assignment "x" (Mul (AVariable "x") (AVariable "i")),
                Assignment "i" (Add (AVariable "i") (Constant 1))
              ]
          ]
  let s = programExec emptyState p
  print p
  print s
