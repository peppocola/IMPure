module Main where

import IMPure.Interpreter (emptyState, programExec)
import IMPure.Parser (parse, parseFailed, getParsedCommands, getRemainingInput)

logo :: IO ()
logo = do
  putStrLn "\n\n"
  putStrLn "                                            😈 IMPure 😈                                  "
  putStrLn "                                                                                           "
  putStrLn "                           :+-`           Giuseppe Colavito            .//                 "
  putStrLn "                           -MNds:.                                .:odNM/                  "
  putStrLn "                            mMMMMmho-`       ``......``       `-+hmMMMMM.                  "
  putStrLn "                            yMMMMMMMMmy/-/oyhdmmmNNmmmddhs+-:sdNMMMMMMMd                   "
  putStrLn "                            /MMMMMMMMMMMMMNmdhyssoossyhdmNMMMMMMMMMMMMMo                   "
  putStrLn "                            `NMMMMMMMMmho:-`            `.:+ymNMMMMMMMM-                   "
  putStrLn "                             hMMMMMms:`                      `-odNMMMMN                    "
  putStrLn "                            `yMMNh/`                             -yNMMm.                   "
  putStrLn "                           :dMMh-                                  .sNMN+`                 "
  putStrLn "                         `oMMm/      `                         `     -dMMh`                "
  putStrLn "                        `yMMh.      -h:                      .y+      `sMMd.               "
  putStrLn "                        yMMh`       -MMh:                  .sNMo        oMMd`              "
  putStrLn "                       +MMd`        -MMMMh-              .sNMMMo         sMMh              "
  putStrLn "                      .NMN.         -MMMMMMh-          .sNMMMMMo         `mMM/             "
  putStrLn "                      sMMs          -MMMMMMMMy-      `sNMMMMMMMo          /MMd             "
  putStrLn "                      mMM-          `::::::::::`     ::::::::::.          `NMM.            "
  putStrLn "                     `MMM`                                                 dMM:            "
  putStrLn "                     `MMN                                                  hMM/            "
  putStrLn "                     `MMM`          `NNNNNNNNNNNNNNNNNNNNNNNNNN/           mMM:            "
  putStrLn "                      dMM/           sMMMMMMMMMMMMMMMMMMMMMMMMd           `MMM`            "
  putStrLn "                      +MMh           `hMMMMMMMMMMMMMMMMMMMMMMm.           oMMy             "
  putStrLn "                      `mMM/           `oNMMMMMMMMMMMMMMMMMMMy.           .NMM-             "
  putStrLn "                       :MMN-            .smMMMMMMMMMMMMMMNy:            `dMMo              "
  putStrLn "                        +MMm-             `:shmNMMMMNmdy/.             `hMMs               "
  putStrLn "                         +NMN/               ``..--..``               -dMMs`               "
  putStrLn "                          :mMNy.                                    `oNMN+                 "
  putStrLn "                           `sNMNo.                                `/dMMh-                  "
  putStrLn "                             -yNMms:`                           -omMMd/`                   "
  putStrLn "                               -smMNds:.                    `-odNMNy:`                     "
  putStrLn "                                 ./ymMMmds+:-..```````.-:/shmMMmh+.                        "
  putStrLn "                                    .:+hdNMMNNmddddddmmNMMNmho:.                           "
  putStrLn "                                        `.-/+syhhhhhhyso/:-`                               "
  putStrLn "                                                                                           "

main :: IO ()
main = do
  logo
  putStrLn "Welcome!"
  putStrLn "Insert the path to the file you want to use!"
  input <- getLine;
  p <- readFile input
  let c = parse p
  if parseFailed c
    then do
      putStrLn "\nParsing failed\n"
      putStrLn "\nRemaining input:\n"
      print (getRemainingInput c)
    else do
      putStrLn "\nParsing success!\n"
      let s = programExec emptyState (getParsedCommands c)
      putStrLn "\nInput Program\n"
      putStrLn p
      putStrLn "\nRepresentation of the program:\n"
      print (getParsedCommands c)
      putStrLn "\nState of the memory:\n"
      print s