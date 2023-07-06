{-| Encapsulate a text error from aeson parsing -}
module Data.Aeson.Error
  ( AesonError, AsAesonError( _AesonError ), throwAsAesonError )
where

import Base1

-- base --------------------------------

import GHC.Generics  ( Generic )

-- deepseq -----------------------------

import Control.DeepSeq  ( NFData )

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

{-| Encapsulate a text error as an AesonError and throw it -}
throwAsAesonError ∷ ∀ ε τ β η . (AsAesonError ε, Printable τ, MonadError ε η) ⇒
                    τ → η β
throwAsAesonError = throwError ∘ (_AesonError #) ∘ aesonError

-- that's all, folks! ----------------------------------------------------------
