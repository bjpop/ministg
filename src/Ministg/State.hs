-----------------------------------------------------------------------------
-- |
-- Module      : Ministg.State
-- Copyright   : (c) 2009 Bernie Pope 
-- License     : BSD-style
-- Maintainer  : bjpop@csse.unimelb.edu.au
-- Stability   : experimental
-- Portability : ghc
--
-- Representation of the state of the ministg evaluator.
-----------------------------------------------------------------------------

module Ministg.State 
   ( Continuation (..)
   , Stack
   , prettyStack 
   , Heap 
   , EvalState (..)
   , Eval
   , initStack
   , initHeap
   , initState
   , pushCallStack
   , setCallStack
   , lookupHeap
   , lookupHeapAtom
   , updateHeap
   , freshVar
   , incStepCount
   , setRule
   , prettyHeapObject
   )
   where

import Control.Monad.State 
import Data.Map as Map hiding (map)
import Data.Set as Set hiding (map)
import Ministg.AST
import Ministg.CallStack (CallStack, push, prettyCallStack)
import Ministg.Pretty 
import Ministg.Options 
   ( Flag (..), defaultMaxSteps, defaultTraceDir
   , probeFlagsFirst, existsFlag, getTraceDir, getMaxTraceSteps )

-- | Stack continuations.
data Continuation
   = CaseCont [Alt] CallStack -- ^ The alternatives of a case expression.
   | UpdateCont Var CallStack -- ^ A variable which points to a thunk to be updated.
   | ArgCont Atom             -- ^ A pending argument (used only by the push-enter model).
   | ApplyToArgs [Atom]       -- ^ Apply the returned function to these arguments (eval-apply only).
   deriving (Eq, Show)

instance FreeVars Continuation where
   freeVars (CaseCont alts _cs) = freeVars alts
   freeVars (UpdateCont var _cs) = Set.singleton var
   freeVars (ArgCont arg) = freeVars arg
   freeVars (ApplyToArgs args) = freeVars args

instance Pretty Continuation where
   pretty (CaseCont alts callStack) 
      = text "case *" <+> braces (vcat (punctuate semi (map pretty alts))) $$
        nest 3 (prettyCallStack callStack)
   pretty (UpdateCont var callStack) 
      = text "upd *" <+> text var $$
        nest 3 (prettyCallStack callStack)
   pretty (ArgCont atom) = text "arg" <+> pretty atom 
   pretty (ApplyToArgs atoms) = parens (char '*' <+> hsep (map pretty atoms))

-- | The evaluation stack. 
type Stack = [Continuation]

prettyStack :: Stack -> Doc
prettyStack stack = (vcat $ map prettyCont stack)
   where
   prettyCont :: Continuation -> Doc
   prettyCont cont = text "-" <+> pretty cont

-- | The heap (mapping variables to objects).
type Heap = Map.Map Var Object

-- | State to be threaded through evaluation.
data EvalState 
   = EvalState 
     { state_unique :: !Int           -- ^ Unique counter for generating fresh variables.
     , state_callStack :: CallStack   -- ^ Function call stack (for debugging).
     , state_stepCount :: !Integer    -- ^ How many steps have been executed.
     , state_lastRule :: !String      -- ^ The most recent evaluation rule applied.
     , state_trace :: Bool            -- ^ Do we want tracing of evaluation steps? 
     , state_maxTraceSteps :: Integer -- ^ Maximum number of evaluation steps to trace.
     , state_traceDir :: String       -- ^ Name of directory to store trace files.
     , state_gc :: Bool               -- ^ Do we want garbage collection?
     , state_traceCallStack :: Bool   -- ^ Do we want the call stack shown in the trace?
     }

-- | Eval monad. Combines State and IO.
type Eval a = StateT EvalState IO a

initState :: [Flag] -> EvalState
initState flags = 
   EvalState 
   { state_unique = 0
   , state_callStack = []
   , state_stepCount = 0 
   , state_lastRule = ""
   , state_trace = existsFlag flags Trace 
   , state_maxTraceSteps = getMaxTraceSteps flags
   , state_traceDir = getTraceDir flags
   , state_gc = not $ existsFlag flags NoGC 
   , state_traceCallStack = existsFlag flags CallStack
   }

initHeap :: Program -> Heap
initHeap = Map.fromList

initStack :: Stack
initStack = []

setRule :: String -> Eval ()
setRule str = do
   lr <- gets state_lastRule
   modify $ \s -> s { state_lastRule = str } 

incStepCount :: Eval ()
incStepCount = do
   sc <- gets state_stepCount
   modify $ \s -> s { state_stepCount = sc + 1 } 

pushCallStack :: String -> Eval ()
pushCallStack str = do
   cs <- gets state_callStack
   modify $ \s -> s { state_callStack = push str cs }

setCallStack :: CallStack -> Eval ()
setCallStack cs = modify $ \s -> s { state_callStack = cs }

-- | Lookup a variable in a heap. If found return the corresponding
-- object, otherwise throw an error (it is a fatal error which can't
-- be recovered from).
lookupHeap :: Var -> Heap -> Object 
lookupHeap var heap = 
   case Map.lookup var heap of
      Nothing -> error $ "undefined variable: " ++ show var
      Just object -> object

-- | Convenience wrapper for lookupHeap, for atoms which happen to be variables.
lookupHeapAtom :: Atom -> Heap -> Object
lookupHeapAtom (Variable var) heap = lookupHeap var heap
lookupHeapAtom other _heap = error $ "lookupHeapAtom called with non variable " ++ show other

-- | Add a new mapping to a heap, or update an existing one.
updateHeap :: Var -> Object -> Heap -> Heap 
updateHeap = Map.insert 

-- | Generate a new unique variable. Uniqueness is guaranteed by using a
-- "$" prefix, which is not allowed in the concrete sytax of ministg programs.
freshVar :: Eval Var
freshVar = do
   u <- gets state_unique
   modify $ \s -> s { state_unique = u + 1 }
   return $ "$" ++ show u

-- XXX not very good for printing large objects, nonetheless it is lazy.
prettyHeapObject :: Heap -> Object -> String 
prettyHeapObject heap (Con constructor args)
   | length args == 0 = constructor
   | otherwise = "(" ++ unwords (constructor : map (prettyHeapAtom heap) args) ++ ")"
prettyHeapObject _heap (Fun {}) = "<function>"
prettyHeapObject _heap (Pap {}) = "<pap>"
prettyHeapObject _heap (Thunk {}) = "<thunk>"
prettyHeapObject _heap BlackHole = "<blackhole>"
prettyHeapObject _heap Error = "<error>"

prettyHeapAtom :: Heap -> Atom -> String 
prettyHeapAtom heap (Literal (Integer i)) = show i
prettyHeapAtom heap (Variable var) = prettyHeapObject heap $ lookupHeap var heap
