module Main where
import IMPure.Parser(parse)
import IMPure.Interpreter (programExec, emptyState)

logo :: IO ()
logo = do {
putStrLn "                                                                                           " ;                                                        
putStrLn "                                                                                           " ;                                                          
putStrLn "                           :+-`                                       .//                  " ;                                                          
putStrLn "                           -MNds:.                                .:odNM/                  " ;                                                          
putStrLn "                            mMMMMmho-`       ``......``       `-+hmMMMMM.                  " ;                                                          
putStrLn "                            yMMMMMMMMmy/-/oyhdmmmNNmmmddhs+-:sdNMMMMMMMd                   " ;                                                          
putStrLn "                            /MMMMMMMMMMMMMNmdhyssoossyhdmNMMMMMMMMMMMMMo                   " ;                                                          
putStrLn "                            `NMMMMMMMMmho:-`            `.:+ymNMMMMMMMM-                   " ;                                                         
putStrLn "                             hMMMMMms:`                      `-odNMMMMN                    " ;                                                          
putStrLn "                            `yMMNh/`                             -yNMMm.                   " ;                                                          
putStrLn "                           :dMMh-                                  .sNMN+`                 " ;                                                          
putStrLn "                         `oMMm/      `                         `     -dMMh`                " ;                                                          
putStrLn "                        `yMMh.      -h:                      .y+      `sMMd.               " ;                                                          
putStrLn "                        yMMh`       -MMh:                  .sNMo        oMMd`              " ;                                                          
putStrLn "                       +MMd`        -MMMMh-              .sNMMMo         sMMh              " ;                                                          
putStrLn "                      .NMN.         -MMMMMMh-          .sNMMMMMo         `mMM/             " ;                 
putStrLn "                      sMMs          -MMMMMMMMy-      `sNMMMMMMMo          /MMd             " ;              
putStrLn "                      mMM-          `::::::::::`     ::::::::::.          `NMM.            " ;                
putStrLn "                     `MMM`                                                 dMM:            " ;                
putStrLn "                     `MMN                                                  hMM/            " ;                   
putStrLn "                     `MMM`          `NNNNNNNNNNNNNNNNNNNNNNNNNN/           mMM:            " ;                
putStrLn "                      dMM/           sMMMMMMMMMMMMMMMMMMMMMMMMd           `MMM`            " ;                                                          
putStrLn "                      +MMh           `hMMMMMMMMMMMMMMMMMMMMMMm.           oMMy             " ;                                                          
putStrLn "                      `mMM/           `oNMMMMMMMMMMMMMMMMMMMy.           .NMM-             " ;                                                          
putStrLn "                       :MMN-            .smMMMMMMMMMMMMMMNy:            `dMMo              " ;                                                          
putStrLn "                        +MMm-             `:shmNMMMMNmdy/.             `hMMs               " ;                                                          
putStrLn "                         +NMN/               ``..--..``               -dMMs`               " ;                                                          
putStrLn "                          :mMNy.                                    `oNMN+                 " ;                                                          
putStrLn "                           `sNMNo.                                `/dMMh-                  " ;                                                          
putStrLn "                             -yNMms:`                           -omMMd/`                   " ;                                                          
putStrLn "                               -smMNds:.                    `-odNMNy:`                     " ;                                                          
putStrLn "                                 ./ymMMmds+:-..```````.-:/shmMMmh+.                        " ;                                                          
putStrLn "                                    .:+hdNMMNNmddddddmmNMMNmho:.                           " ;                                                          
putStrLn "                                        `.-/+syhhhhhhyso/:-`                               " ; 
putStrLn "                                                                                           " ;                                                        
}     

exec :: IO()
exec = do
    p <- readFile "test.pure"
    let c = parse p
    let s = programExec emptyState c
    --print c
    putStrLn "State of the memory:"
    print s

main :: IO()
main = do
    logo;
    exec;