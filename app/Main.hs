module Main where

import IMPure.Interpreter (emptyState, programExec)
import IMPure.Parser (parse)

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

exec :: IO ()
exec = do
  p <- readFile "test.pure"
  let c = parse p
  let s = programExec emptyState c
  putStrLn "\nInput Program\n"
  putStrLn p
  putStrLn "\nRepresentation of the program:\n"
  print c
  putStrLn "\nState of the memory:\n"
  print s

main :: IO ()
main = do
  logo
  exec