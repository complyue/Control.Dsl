
:set -XGADTs
:set -XMultiParamTypeClasses
:set -XFlexibleInstances
:set -XFlexibleContexts
:set -XRebindableSyntax
:set -XTypeApplications

import qualified Prelude
import Prelude hiding ((>>), (>>=), return, fail)
import Control.Dsl
import Control.Dsl.State.Get
import Control.Dsl.Yield
import Control.Dsl.Return
import Data.Void

:{
f = do
  Yield "foo"
  config <- Get @Bool
  when config $ do
    Yield "bar"
    return ()
  return "baz"
:}

-- @f@ is a @do@ block that contains keywords of
-- 'Control.Dsl.State.Get.Get',
-- 'Control.Dsl.Yield.Yield',
-- and 'Control.Dsl.Return.return'.
-- With the help of built-in 'PolyCont' instances for those keywords,
-- @f@ can be used as a function that accepts a boolean parameter.

f False :: [String]
-- ["foo","baz"]

f True :: [String]
-- ["foo","bar","baz"]

-- In fact, @f@ can be any type
-- as long as 'PolyCont' instances for involved keywords are provided.

:type f
-- f :: (PolyCont (Yield [Char]) r (),
--       PolyCont (Return [Char]) r Void, PolyCont Get r Bool) =>
--      r

-- For example, @f@ can be interpreted as an impure @IO ()@,
-- providing the following instances:

:{
instance PolyCont (Yield String) (IO ()) () where
  runPolyCont (Yield a) = (Prelude.>>=) (putStrLn $ "Yield " ++ a)
instance PolyCont Get (IO ()) Bool where
  runPolyCont Get f = putStrLn "Get" Prelude.>> f False
instance PolyCont (Return String) (IO ()) Void where
  runPolyCont (Return r) _ = putStrLn $ "Return " ++ r
:}

f :: IO ()
-- Yield foo
-- Get
-- Return baz
