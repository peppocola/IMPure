module Main where
import IMPure.Parser(Parser, parse)
import IMPure.Interpreter (programExec, emptyState)

main :: IO ()
main = do
    p <- readFile "test.pure"
    let c = parse p
    let s = programExec emptyState c
    print c
    print s