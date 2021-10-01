module Main where
import DSG.Domain.Workspace (mkWorkspace, addTaskToBacklog)
import DSG.Domain.Task (mkTask)

main :: IO ()
main = do 
  putStrLn "Daily Standup Generator (DSG) v0.1.0"
  ws <- mkWorkspace "Test Workspace"
  first <- mkTask "first task"
  second <- mkTask "second task"
  third <- mkTask "third task"

  -- adding in the order second, first, third to make sure the sorting is also working
  let result = addTaskToBacklog third 
                $ addTaskToBacklog first 
                $ addTaskToBacklog second ws

  print result
        
