module Main where
import Grammar
  ( AExp (Add, Constant, AVariable, Mul),
    Operator (Le),
    BExp (Comparison),
    Command (Assignment, While, VariableDeclaration),
    Program (Program)
  )
import Interpreter
  ( programExec, emptyState)
main :: IO ()
main = do
 let p =
        Program
          [VariableDeclaration "i" (Constant 1), VariableDeclaration "n" (Constant 5), VariableDeclaration "x" (Constant 1),
          While
              (Comparison (AVariable "i") (AVariable "n") Le)
              [ Assignment "x" (Mul (AVariable "x") (AVariable "i")),
                Assignment "i" (Add (AVariable "i") (Constant 1))
              ]
          ]
 let s = programExec emptyState p
 print p
 print s

