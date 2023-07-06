{-# LANGUAGE DeriveAnyClass #-}

module Data.Aeson.Error
  ( AesonError, AsAesonError( _AesonError ), throwAsAesonError )
where

import Base1T

-- base --------------------------------

import Control.Exception  ( Exception )
import Control.Monad      ( return )
import Data.Eq            ( Eq( (==) ) )
import Data.Function      ( ($), (&), id )
import Data.Maybe         ( maybe )
import GHC.Generics       ( Generic )
import GHC.Stack          ( CallStack, HasCallStack, callStack )
import Text.Read          ( Read, readMaybe )
import Text.Show          ( Show( show ) )

-- data-textual ------------------------

import Data.Textual  ( Printable( print ), toString, toText )

-- deepseq -----------------------------

import Control.DeepSeq  ( NFData )

-- fpath -------------------------------

import FPath.Error.FPathError  ( AsFPathError( _FPathError )
                               , FPathError, FPathIOError )

-- lens --------------------------------

import Control.Lens.Lens    ( lens )
import Control.Lens.Prism   ( Prism', prism' )
import Control.Lens.Review  ( (#) )

-- monaderror-io -----------------------

import MonadError.IO.Error  ( AsIOError( _IOError ), IOError )

-- monadio-plus ------------------------

import MonadIO.Error.CreateProcError  ( AsCreateProcError( _CreateProcError )
                                      , CreateProcError )
import MonadIO.Error.ProcExitError    ( AsProcExitError( _ProcExitError )
                                      , ProcExitError )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError, throwError )

-- text --------------------------------

import Data.Text  ( Text )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

--------------------------------------------------------------------------------

{-| an error in cmdline calling args & options -}
data AesonError = AesonError { _txt ∷ Text, _callstack ∷ CallStack }
  deriving (Generic,NFData,Show)

----------------------------------------

instance Exception AesonError

----------------------------------------

instance Eq AesonError where
  (AesonError a _) == (AesonError b _) = a == b

----------------------------------------

instance HasCallstack AesonError where
  callstack = lens _callstack (\ eu cs → eu { _callstack = cs })

----------------------------------------

{-| prisms including @AesonError -}
class AsAesonError ε where
  _AesonError ∷ Prism' ε AesonError

--------------------

instance AsAesonError AesonError where
  _AesonError = id

--------------------

instance Printable AesonError where
  print = P.text ∘ _txt

----------------------------------------

{-| create an @AsUsageError@ from a @ToText@ -}
aesonError ∷ ∀ τ ε . (AsAesonError ε, Printable τ, HasCallStack) ⇒ τ → ε
aesonError t = _AesonError # AesonError (toText t) callStack

----------------------------------------

throwAsAesonError ∷ ∀ ε τ β η . (AsAesonError ε, Printable τ, MonadError ε η) ⇒
                    τ → η β
throwAsAesonError = throwError ∘ (_AesonError #) ∘ aesonError

-- that's all, folks! ----------------------------------------------------------
