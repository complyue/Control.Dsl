{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}


module CDSL where


import qualified Prelude
import           Prelude                 hiding ( (>>)
                                                , (>>=)
                                                , return
                                                , fail
                                                )

import           Data.Void
import           System.IO
import           System.IO.Temp

import           Control.Dsl


import           Control.Dsl.PolyCont
import           Control.Dsl.Cont
import           Control.Dsl.Return
import           Control.Dsl.Empty
import           Control.Dsl.Yield
import           Control.Dsl.Monadic
import           Control.Dsl.State.Get
import           Control.Dsl.State.Put
import           Control.Dsl.State






data MaxLengthConfig r a where MaxLengthConfig ::MaxLengthConfig r Int
data GetLine r a where GetLine ::GetLine r String
data PutStrLn r a where PutStrLn ::String -> PutStrLn r ()


dslBlock = do
    maxLength <- MaxLengthConfig
    line1     <- GetLine
    line2     <- GetLine
    when (length line1 + length line2 > maxLength) $ do
        PutStrLn "The input is too long"
        fail "Illegal input"
    PutStrLn ("The input is " ++ line1 ++ " and " ++ line2)
    return ()



type PureInterpreter = Int -> [String] -> Cont [String] IOError


instance PolyCont MaxLengthConfig PureInterpreter Int where
    runPolyCont MaxLengthConfig = runPolyCont Get



instance PolyCont PutStrLn PureInterpreter () where
    runPolyCont (PutStrLn s) = runPolyCont (Yield s)



instance PolyCont (Return ()) PureInterpreter Void where
    runPolyCont (Return ()) = runPolyCont Empty


-- The above three PolyCont instances are implemented as forwarders to other existing keywords.


instance PolyCont GetLine PureInterpreter String where
    runPolyCont k = runCont $ do
        x : xs <- Get @[String]
        Put xs
        return x




runPurely = dslBlock :: PureInterpreter

-- type EffectfulInterpreter = Handle -> IO ()
-- instance PolyCont GetLine EffectfulInterpreter String where
--     runPolyCont GetLine = runCont $ do
--       h <- Get
--       line <- Monadic (hGetLine h)
--       return line

-- runEffectfully = dslBlock :: EffectfulInterpreter

