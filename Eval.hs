{-# language BangPatterns #-}


module Eval(eval, apply, mangle, process,rCdr,rCar,primCudder,unquoted) where

import Prelude hiding(catch)
import SyntaxTree
import MiscUtil
import Control.Monad.Error
import Control.Monad.State.Strict
import Control.Exception
import Data.IORef
import System.IO(hPutStr, stderr)

{-
prosessoi jäsennyspuun suoritettavaan muotoon, eli:
- erikoismuotojen tunnistus ja muunnokset
- häntäkutsujen merkitseminen
- funktioiden ympäristöjen merkitseminen
-}
mangle :: AST -> ASTComputation
mangle = annotateFrames . (annotateTails False) . process

annotateFrames :: AST -> ASTComputation
annotateFrames (Sequence seq) = mapM annotateFrames seq >>= return . Sequence
annotateFrames (Block block) = mapM annotateFrames block >>= return . Block
annotateFrames (Define var body) = annotateFrames body >>= \body' -> return (Define var body')
annotateFrames (If p t f) = do
  env <- get
  p' <- annotateFrames p
  t' <- annotateFrames t
  f' <- annotateFrames f
  return $! If p' t' f'
annotateFrames (Macro body args _) = annotateFunc Macro body args
annotateFrames (Proc body args _) = annotateFunc Proc body args
annotateFrames x = return x

annotateFunc f body args = do
  (env:rest) <- get
  frame <- liftIO emptyFrame
  let newEnv = pushEnv frame env
  put (newEnv:rest)
  body' <- annotateFrames body
  put (env:rest)
  return $! f body' args newEnv

annotateTails :: Bool -> AST -> AST
annotateTails x (If p t f) = If p (annotateTails x t) (annotateTails x f)
annotateTails True (Sequence seq) = Sequence (Sym "tailcall" : seq)
annotateTails False s@(Sequence seq) = s
annotateTails x (Proc body args env) = Proc (annotateTails True body) args env
annotateTails x (Define var val) = Define var (annotateTails x val)
annotateTails x (Block b) = Block $ mapP (annotateTails False) (annotateTails x) b
annotateTails _ a = a

-- mapP f g [a,b,..,y,z] = [f a, f b, ..., f y, g z]
mapP _ _ [] = []
mapP _ g (x:[]) = g x : []
mapP f g (x:y:xs) = (f x) : (mapP f g (y:xs))

letToLambda  :: [AST] -> AST -> AST
letToLambda bindings body =
    let
        (vars,vals) = unzip (map f bindings)
        f (Sequence [Sym x, val]) = (x, val)
    in
      Sequence $ (makeFunction vars body):vals

-- (let* ((x e1) (y e2)) ..) => (lambda (x) ((lambda (y) ... ) e2) e1)
letStarToLambda :: [AST] -> AST -> AST
letStarToLambda bindings body = f bindings
    where
      f [] = body
      f (Sequence [Sym var, value] : bindings) =  Sequence $ makeFunction [var] (f bindings) : value : []
      f x = error (show x)

condToIf ((Sequence (p:(Sym "=>"):t):rest)) = condToIf ((Sequence (p:t):rest))
condToIf ((Sequence (Sym "else":vals)):_) = condToIf [Boolean True, maybeBlock vals]
condToIf (Boolean True:val:_) = val
condToIf (Sequence (p:t):rest) =
    let
        p' = process p
        f = if null rest then Boolean False else condToIf rest
    in
      If p' (maybeBlock t) f
--condToIf x = x

maybeBlock (x:[]) = process x
maybeBlock xs = Block (map process xs)

-- prosessoi erikoismuodot jne, jätä funktioiden ympäristöt määrittelemättä
process :: AST -> AST
process (Sequence [Sym "if", pred, tBranch]) =
    process $ Sequence [Sym "if", pred, tBranch, Boolean False]
process (Sequence [Sym "if", pred, tBranch, fBranch]) =
    If (process pred) (process tBranch) (process fBranch)
process (Sequence (Sym "cond":conds)) = condToIf conds
process (Sequence [Sym "define", Sym v, value]) =
    Define v (process value)
process (Sequence (Sym "define":Sequence ((Sym name):args):body)) =
    Define name $ process $ Sequence ((Sym "lambda") : (Sequence args) : body)
process (Sequence (Sym "define":(Improper ((Sym name):args) last):body)) =
    Define name $ process $ Sequence ((Sym "lambda") : (Improper args last) : body)
process (Sequence (Sym "lambda" : Sym arg : body)) = makeFunction ['.':arg] (maybeBlock body)
process (Sequence (Sym "lambda" : Improper args (Sym x) : body)) = makeFunction ((processFunArgs args) ++ ['.':x]) (maybeBlock body)
process (Sequence (Sym "lambda" : Sequence args : body)) = makeFunction (processFunArgs args) (maybeBlock body)
process (Sequence (Sym ('c':c):rest)) | length c > 6 && isCaddy c = Sequence $
                                 primCudder (filter (`elem` "ad") c) : (map process rest)
process (Sequence (Sym "let":Sequence bindings: body)) =
    letToLambda bindings (maybeBlock body)
process (Sequence (Sym "let*":Sequence bindings: body)) =
    letStarToLambda bindings (maybeBlock body)
process (Sequence [Sym "quote", ast]) = process $ Quoted ast
process (Sequence (Sym "begin" : block)) = Block (map process block)
-- (define-macro (name args) body) => (Define name (Macro args body))
process (Sequence (Sym "define-macro":Sequence ((Sym name):args):body)) = Define name $ makeMacro (processFunArgs args) (maybeBlock body)
process (Sequence (Sym "define-macro":(Improper ((Sym name):args) last):body)) =
    Define name $ makeMacro (processFunArgs (args ++ [Sym "."] ++ [last])) (maybeBlock body)
process (Sequence x) = Sequence (map process x)
process q@(Quoted _) = processQuoted q
process x = x

processFunArgs (Sym ".":Sym x:xs) = ('.':x) : processFunArgs xs
processFunArgs (Sym x:xs) = x : processFunArgs xs
processFunArgs [] = []

processQuoted (Quoted v@(Value _)) = v
processQuoted (Quoted v@(Boolean _)) = v
processQuoted (Quoted v@(Str _)) = v
processQuoted v@(Sym _) = Quoted v
processQuoted (Sequence x) = Sequence $ map processQuoted x
processQuoted x = x

unquoted (Sequence x) = Sequence (map unquoted x)
unquoted (Quoted x) = unquoted x
unquoted x = x

makeFunction args body = Proc body args undefined
makeMacro args body = Macro body args undefined

isCaddy xs = f xs where
    f ('a':xs) = f xs
    f ('d':xs) = f xs
    f ('r':[]) = True
    f _ = False

rCar :: AST -> ASTComputation
rCar = f where
    f x = do
      case x of
        (Quoted (Sequence (z:_))) -> return z
        (Sequence (z:_)) -> return z
        (Improper (z:_) _) -> return z
        (Improper [] z) -> return z
        _ -> throwError $ "car: invalid arguments -- " ++ show x

rCdr :: AST -> ASTComputation
rCdr = f where
    f x = do
      case x of
        (Quoted (Sequence (_:zs))) -> return (Sequence zs)
        (Sequence (_:zs)) -> return (Sequence zs)
        (Improper (_:[]) x) -> return x
        (Improper (_:zs) x) -> return (Improper zs x)
        _ -> throwError "cdr: invalid arguments"


primCudder xs =
    let
        func = caddadadadra xs
        f (x:[]) = eval x >>= func
        f _ = throwError "c[ad]{2,}r: invalid arguments"
        caddadadadra = foldr1 (<=<) . map cudderer where
            cudderer 'a' = rCar
            cudderer 'd' = rCdr
    in BuiltInProc f

bindArguments (".":[]) vals = makeSymRef (Sequence vals) >>= insertSym "."
bindArguments (('.':x):[]) vals = makeSymRef (Sequence vals) >>= insertSym x
bindArguments [] (val:vals) = throwError "Too many arguments"
bindArguments (arg:args) [] = throwError "Not enough arguments"
bindArguments [] [] = return ()
bindArguments (arg:args) (val:vals) = do
  makeSymRef val >>= insertSym arg
  bindArguments args vals

{-# INLINE eval #-}
eval :: AST -> ASTComputation
eval !(Define id (Sym other)) = do
  ref <- findVarRef other
  val <- liftIO $ readIORef ref
  if isReferenceValue val
    then insertSym id ref
    else makeSymRef val >>= insertSym id
  return $! Sym other

eval !(Define id expr) = do
  eval expr >>= (makeSymRef >=> insertSym id) >>=> return
      where
        x >>=> y = \z -> x z >> y z
eval !(Sym s) = findVar s
eval !(If pred tBr fBr) = do
  res <- eval pred
  eval $! if res /= Boolean False
          then tBr
          else fBr
eval !(Sequence (Sym "tailcall":fn:args)) = eval fn >>= (`tapply` args)
    where
      {-# INLINE tapply #-}
      tapply :: AST -> [AST] -> ASTComputation
      tapply proc@(Proc _ _ functionEnv) !args = do
        args' <- mapM' eval args
        (_:currentEnv) <- get
        put (functionEnv:currentEnv)
        (Proc body fargs functionEnv') <- annotateFrames proc
        put (functionEnv':currentEnv)
        bindArguments fargs args'
        eval body
      tapply x args = apply x args
eval !(Sequence (fn:args)) = eval fn >>= (`apply` args)
eval !(Block x) = evalBlock x where
    evalBlock (x:[]) = eval x
    evalBlock (x:xs) = eval x >> (evalBlock xs)
--eval (Quoted x) = return x
eval !v = return v

apply :: AST -> [AST] -> ASTComputation
apply (Macro margs body env) args = do
  res <- applyHelper (Proc margs body env) args
  mangle (unquoted res) >>= eval

apply proc@(Proc _ _ functionEnv) !args = do
  args' <- mapM' eval args
  applyHelper proc args'

apply (BuiltInProc f) args = f args
apply x args = throwError $ "error in apply: (" ++ (show x) ++ " " ++ (show args) ++ ")"

applyHelper proc@(Proc _ _ functionEnv) args = do
  currentEnv <- get
  put (functionEnv:currentEnv)

  -- joka kerta kun kutsutaan, pitää annotoida ympäristöt uudelleen sisäfunktioihin :(
  (Proc body fargs functionEnv') <- annotateFrames proc

  -- nykyinen ympäristö = funktion ympäristö
  put (functionEnv':currentEnv)

  bindArguments fargs args
  result <- eval body
  -- vanha ympäristö taas käyttöön
  put currentEnv
  return result

