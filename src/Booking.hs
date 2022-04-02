{-# LANGUAGE OverloadedStrings #-}

module Booking
    (
        book
    ) where 


import System.Directory
import System.IO
import qualified Data.ByteString as BS
import System.Console.ANSI
import Data.Aeson

import Config
import DataType
import Interface



book :: IO()
book = do
    hSetBuffering stdout NoBuffering 
    clearScreen
    fileExist <- doesFileExist cnfName
    if not fileExist then
         writeFile cnfName ""
    else return ()
    contents <- BS.readFile cnfName 
    let loadedTables = decodeStrict contents :: Maybe Tables 
        tables = case loadedTables of
              Just a -> a
              Nothing -> []
    runner tables Role Nothing Nothing False    

