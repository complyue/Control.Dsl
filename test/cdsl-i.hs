:set -XGADTs
:set -XFlexibleContexts
:set -XFlexibleInstances
:set -XMultiParamTypeClasses
:set -XTypeSynonymInstances
:set -XRebindableSyntax
:set -XUndecidableInstances
:set -XTypeApplications


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





data MaxLengthConfig r a where MaxLengthConfig :: MaxLengthConfig r Int
data GetLine r a where GetLine :: GetLine r String
data PutStrLn r a where PutStrLn :: String -> PutStrLn r ()

:{
dslBlock = do
  maxLength <- MaxLengthConfig
  line1 <- GetLine
  line2 <- GetLine
  when (length line1 + length line2 > maxLength) $ do
    PutStrLn "The input is too long"
    fail "Illegal input"
  PutStrLn ("The input is " ++ line1 ++ " and " ++ line2)
  return ()
:}


type PureInterpreter = Int -> [String] -> Cont [String] IOError

:{
instance PolyCont MaxLengthConfig PureInterpreter Int where
  runPolyCont MaxLengthConfig = runPolyCont Get
:}

:{
instance PolyCont PutStrLn PureInterpreter () where
  runPolyCont (PutStrLn s) = runPolyCont (Yield s)
:}

:{
instance PolyCont (Return ()) PureInterpreter Void where
  runPolyCont (Return ()) = runPolyCont Empty
:}

-- The above three PolyCont instances are implemented as forwarders to other existing keywords.

:{
instance PolyCont GetLine PureInterpreter String where
  runPolyCont k = runCont $ do
    x : xs <- Get @[String]
    Put xs
    return x
:}


-- === Running the DSL purely

runPurely = dslBlock :: PureInterpreter

errorHandler e = ["(handled) " ++ show e]
runCont (runPurely 80 ["LINE_1", "LINE_2"]) errorHandler
-- ["The input is LINE_1 and LINE_2"]

longInput = [replicate 40 '*', replicate 41 '*']
runCont (runPurely 80 longInput) errorHandler
-- ["The input is too long","(handled) user error (Illegal input)"]

runCont (runPurely 80 ["ONE_LINE"]) errorHandler
-- ["(handled) user error (Pattern match failure in do expression at <interactive>..."]

-- === Creating an effectful interpreter

-- Alternatively, @dslBlock@ can run effectfully by providing effectful
-- 'Control.Dsl.PolyCont.PolyCont' instances.

type EffectfulInterpreter = Handle -> IO ()

:{
instance PolyCont GetLine EffectfulInterpreter String where
  runPolyCont GetLine = runCont $ do
    h <- Get
    line <- Monadic (hGetLine h)
    return line
:}

-- 'Control.Dsl.Monadic.Monadic' is a built-in keyword to perform old-fashioned
-- monadic action in a DSL @do@ block.

-- Other keywords can be used together with 'Control.Dsl.Monadic.Monadic'.
-- No monad transformer is required.

:{
instance PolyCont MaxLengthConfig (IO ()) Int where
  runPolyCont MaxLengthConfig f = f 80
:}

:{
instance PolyCont PutStrLn (IO ()) () where
  runPolyCont (PutStrLn s) = (Prelude.>>=) (putStrLn s)
:}

:{
instance PolyCont (Return IOError) (IO ()) Void where
  runPolyCont (Return e) _ = hPutStrLn stderr (show e)
:}

-- The above three 'Control.Dsl.PolyCont.PolyCont' instances are not directly
-- implemented for @EffectfulInterpreter@.
-- Instead, they are implemented for @IO ()@.
-- Then, instances for @EffectfulInterpreter@ can be automatically derived from
-- instances for @IO ()@.
-- There are two built-in 'Control.Dsl.PolyCont.PolyCont' derivation rules,
-- for 'Control.Dsl.Cont.Cont' and 'State', respectively.
-- What interesting is that 'State' is defined as plain function,
-- which exactly matches the type of @EffectfulInterpreter@.

-- === Running the DSL effectfully

runEffectfully = dslBlock :: EffectfulInterpreter

:{
withSystemTempFile "tmp-input-file" $ \_ -> \h -> do
  Monadic $ hPutStrLn h "LINE_1"
  Monadic $ hPutStrLn h "LINE_2"
  Monadic $ hSeek h AbsoluteSeek 0
  runEffectfully h
:}
-- The input is LINE_1 and LINE_2



