{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RebindableSyntax #-}

module Main
    ( main
    )
where

import           Import
import           RIO.Process

import           CDSL

main :: IO ()
main = do
    lo <- logOptionsHandle stderr True
    pc <- mkDefaultProcessContext
    withLogFunc lo $ \lf ->
        let app = App { appLogFunc        = lf
                      , appProcessContext = pc
                      , appOptions        = Options True True
                      }
        in  runRIO app run

run :: RIO App ()
run = do
    logInfo "starting ..."

    logInfo "done."

