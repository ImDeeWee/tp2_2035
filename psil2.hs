-- TP-1  --- Implantation d'une sorte de Lisp          -*- coding: utf-8 -*-
{-# OPTIONS_GHC -Wall #-}
--
-- Ce fichier défini les fonctionalités suivantes:
-- - Analyseur lexical
-- - Analyseur syntaxique
-- - Pretty printer
-- - Implantation du langage

---------------------------------------------------------------------------
-- Importations de librairies et définitions de fonctions auxiliaires    --
---------------------------------------------------------------------------

import Text.ParserCombinators.Parsec -- Bibliothèque d'analyse syntaxique.
import Data.Char                -- Conversion de Chars de/vers Int et autres.
import System.IO                -- Pour stdout, hPutStr

---------------------------------------------------------------------------
-- 1ère représentation interne des expressions de notre langage           --
---------------------------------------------------------------------------
data Sexp = Snil                        -- La liste vide
          | Scons Sexp Sexp             -- Une paire
          | Ssym String                 -- Un symbole
          | Snum Int                    -- Un entier
          -- Génère automatiquement un pretty-printer et une fonction de
          -- comparaison structurelle.
          deriving (Show, Eq)

-- Exemples:
-- (+ 2 3)  ==  (((() . +) . 2) . 3)
--          ==>  Scons (Scons (Scons Snil (Ssym "+"))
--                            (Snum 2))
--                     (Snum 3)
--
-- (/ (* (- 68 32) 5) 9)
--     ==  (((() . /) . (((() . *) . (((() . -) . 68) . 32)) . 5)) . 9)
--     ==>
-- Scons (Scons (Scons Snil (Ssym "/"))
--              (Scons (Scons (Scons Snil (Ssym "*"))
--                            (Scons (Scons (Scons Snil (Ssym "-"))
--                                          (Snum 68))
--                                   (Snum 32)))
--                     (Snum 5)))
--       (Snum 9)

---------------------------------------------------------------------------
-- Analyseur lexical                                                     --
---------------------------------------------------------------------------

pChar :: Char -> Parser ()
pChar c = do { _ <- char c; return () }

-- Les commentaires commencent par un point-virgule et se terminent
-- à la fin de la ligne.
pComment :: Parser ()
pComment = do { pChar ';'; _ <- many (satisfy (\c -> not (c == '\n')));
                (pChar '\n' <|> eof); return ()
              }
-- N'importe quelle combinaison d'espaces et de commentaires est considérée
-- comme du blanc.
pSpaces :: Parser ()
pSpaces = do { _ <- many (do { _ <- space ; return () } <|> pComment);
               return () }

-- Un nombre entier est composé de chiffres.
integer     :: Parser Int
integer = do c <- digit
             integer' (digitToInt c)
          <|> do _ <- satisfy (\c -> (c == '-'))
                 n <- integer
                 return (- n)
    where integer' :: Int -> Parser Int
          integer' n = do c <- digit
                          integer' (10 * n + (digitToInt c))
                       <|> return n

-- Les symboles sont constitués de caractères alphanumériques et de signes
-- de ponctuations.
pSymchar :: Parser Char
pSymchar = alphaNum <|> satisfy (\c -> c `elem` "!@$%^&*_+-=:|/?<>")
pSymbol :: Parser Sexp
pSymbol= do { s <- many1 (pSymchar);
              return (case parse integer "" s of
                        Right n -> Snum n
                        _ -> Ssym s)
            }

---------------------------------------------------------------------------
-- Analyseur syntaxique                                                  --
---------------------------------------------------------------------------

-- La notation "'E" est équivalente à "(shorthand-quote E)"
-- La notation "`E" est équivalente à "(shorthand-backquote E)"
-- La notation ",E" est équivalente à "(shorthand-comma E)"
pQuote :: Parser Sexp
pQuote = do { c <- satisfy (\c -> c `elem` "'`,"); pSpaces; e <- pSexp;
              return (Scons
                      (Scons Snil
                             (Ssym (case c of
                                     ',' -> "shorthand-comma"
                                     '`' -> "shorthand-backquote"
                                     _   -> "shorthand-quote")))
                      e) }

-- Une liste (Tsil) est de la forme ( [e .] {e} )
pTsil :: Parser Sexp
pTsil = do _ <- char '('
           pSpaces
           (do { _ <- char ')'; return Snil }
            <|> do hd <- (do e <- pSexp
                             pSpaces
                             (do _ <- char '.'
                                 pSpaces
                                 return e
                              <|> return (Scons Snil e)))
                   pLiat hd)
    where pLiat :: Sexp -> Parser Sexp
          pLiat hd = do _ <- char ')'
                        return hd
                 <|> do e <- pSexp
                        pSpaces
                        pLiat (Scons hd e)

-- Accepte n'importe quel caractère: utilisé en cas d'erreur.
pAny :: Parser (Maybe Char)
pAny = do { c <- anyChar ; return (Just c) } <|> return Nothing

-- Une Sexp peut-être une liste, un symbol ou un entier.
pSexpTop :: Parser Sexp
pSexpTop = do { pTsil <|> pQuote <|> pSymbol
                <|> do { x <- pAny;
                         case x of
                           Nothing -> pzero
                           Just c -> error ("Unexpected char '" ++ [c] ++ "'")
                       }
              }

-- On distingue l'analyse syntaxique d'une Sexp principale de celle d'une
-- sous-Sexp: si l'analyse d'une sous-Sexp échoue à EOF, c'est une erreur de
-- syntaxe alors que si l'analyse de la Sexp principale échoue cela peut être
-- tout à fait normal.
pSexp :: Parser Sexp
pSexp = pSexpTop <|> error "Unexpected end of stream"

-- Une séquence de Sexps.
pSexps :: Parser [Sexp]
pSexps = do pSpaces
            many (do e <- pSexpTop
                     pSpaces
                     return e)

-- Déclare que notre analyseur syntaxique peut-être utilisé pour la fonction
-- générique "read".
instance Read Sexp where
    readsPrec _ s = case parse pSexp "" s of
                      Left _ -> []
                      Right e -> [(e,"")]

---------------------------------------------------------------------------
-- Sexp Pretty Printer                                                   --
---------------------------------------------------------------------------

showSexp' :: Sexp -> ShowS
showSexp' Snil = showString "()"
showSexp' (Snum n) = showsPrec 0 n
showSexp' (Ssym s) = showString s
showSexp' (Scons e1 e2) = showHead (Scons e1 e2) . showString ")"
    where showHead (Scons Snil e') = showString "(" . showSexp' e'
          showHead (Scons e1' e2')
            = showHead e1' . showString " " . showSexp' e2'
          showHead e = showString "(" . showSexp' e . showString " ."

-- On peut utiliser notre pretty-printer pour la fonction générique "show"
-- (utilisée par la boucle interactive de GHCi).  Mais avant de faire cela,
-- il faut enlever le "deriving Show" dans la déclaration de Sexp.
{-
instance Show Sexp where
    showsPrec p = showSexp'
-}

-- Pour lire et imprimer des Sexp plus facilement dans la boucle interactive
-- de Hugs/GHCi:
readSexp :: String -> Sexp
readSexp = read
showSexp :: Sexp -> String
showSexp e = showSexp' e ""

---------------------------------------------------------------------------
-- Représentation intermédiaire Lexp                                     --
---------------------------------------------------------------------------

type Var = String

data Lexp = Lnum Int            -- Constante entière.
          | Lvar Var            -- Référence à une variable.
          | Lproc Var [Lexp]    -- Fonction anonyme prenant un argument.
          | Ldo Lexp Lexp       -- Appel de fonction, avec un argument.
          | Lnull               -- Constructeur de liste vide.
          | Lnode Lexp Lexp     -- Constructeur de liste.
          | Lcase Lexp [Lexp] Var Var [Lexp] -- Expression conditionnelle.
          -- Déclaration d'une liste de variables qui peuvent être
          -- mutuellement récursives.
          | Ldef [(Var, Lexp)] [Lexp]
          deriving (Show, Eq)

-- Première passe simple qui analyse une Sexp et construit une Lexp équivalente.
s2l :: Sexp -> Lexp
s2l (Snum n) = Lnum n
s2l (Ssym "null") = Lnull
s2l (Ssym s) = Lvar s
s2l (Scons se1 se2) = scons2l se1 [se2]
s2l se = error ("Expression Psil inconnue: " ++ showSexp se)

scons2l :: Sexp -> [Sexp] -> Lexp
-- ¡¡COMPLÉTER: ajuster à la nouvelle syntaxe!!
scons2l (Scons se1 se2) sargs = scons2l se1 (se2 : sargs)
scons2l Snil [Ssym "node", se1, se2] = Lnode (s2l se1) (s2l se2)
scons2l Snil (Ssym "node" : _sargs)
  = error "Nombre incorrect d'arguments passés à 'node'"
scons2l Snil (Ssym "seq" : sargs) = foldr Lnode Lnull (map s2l sargs)
scons2l Snil [Ssym "proc", sargs, sbody]
  = let loop Snil body = body
        loop (Scons sargs' sarg) body = loop sargs' (Lproc (s2l sarg) body)
        loop se _ = error ("Arguments formels invalides: " ++ showSexp se)
    in loop sargs (s2l sbody)
scons2l Snil (Ssym "proc" : _sargs)
  = error "Nombre incorrect d'arguments passés à 'proc'"
scons2l Snil [Ssym "def", sdefs, sbody]
  = Ldef (s2d sdefs) (s2l sbody)
scons2l Snil (Ssym "def" : _sargs)
  = error "Nombre incorrect d'arguments passés à 'def'"
scons2l Snil (Ssym "case" : se : sbranches)
  = foldr (\ sbranch e' ->
           case e' of
             Lcase e enull x1 x2 enode
               -> case sbranch of
                    Scons (Scons Snil (Ssym "null")) senull
                      -> Lcase e (s2l senull) x1 x2 enode
                    Scons (Scons Snil
                                 (Scons (Scons (Scons Snil (Ssym "node")) sx1)
                                        sx2))
                          senode
                      -> Lcase e enull (s2v sx1) (s2v sx2) (s2l senode)
                    _ -> error ("Branche invalide: " ++ showSexp sbranch)
             _ -> error "Erreur interne dans 'case'")
          (Lcase (s2l se)
                 (Lvar "<branche-null-manquante>")
                 "<dummy>" "<dummy>" (Lvar "<branche-node-manquante>"))
          sbranches
scons2l Snil (se : sargs) = foldl Ldo (s2l se) (map s2l sargs)
scons2l se _ = error ("Tête de liste impropre: " ++ showSexp se)

s2v :: Sexp -> Var
s2v (Ssym x) = x
s2v se = error ("Pas un identifiant: " ++ (showSexp se))

s2d :: Sexp -> [(Var, Lexp)]
s2d Snil = []
s2d (Scons sdefs (Scons (Scons Snil sd) se))
  = let loop (Scons Snil svar) body = (s2v svar, body)
        loop (Scons sd' svar) body = loop sd' (Lproc (s2v svar) body)
        loop svar body = (s2v svar, body)
    in loop sd (s2l se) : s2d sdefs
s2d se = error ("Definitions invalides: " ++ showSexp se)

---------------------------------------------------------------------------
-- Représentation du contexte d'exécution                                --
---------------------------------------------------------------------------

-- Type des valeurs manipulées à l'exécution.
data Value = Vnum Int
           | Vnil
           | Vcons Value Value
           | Vop (Value -> IO Value)
           | Vclosure VEnv Var [Lexp]

instance Show Value where
    showsPrec p (Vnum n) = showsPrec p n
    showsPrec _ Vnil = showString "[]"
    showsPrec p (Vcons v1 v2) =
        let showTail Vnil = showChar ']'
            showTail (Vcons v1' v2') =
                showChar ' ' . showsPrec p v1' . showTail v2'
            showTail v = showString " . " . showsPrec p v . showChar ']'
        in showChar '[' . showsPrec p v1 . showTail v2
    showsPrec _ (Vop _) = showString "<primitive>"
    showsPrec _ (Vclosure _ _ _) = showString "<fonction>"

type VEnv = [(Var, Value)]

-- L'environnement initial qui contient les fonctions prédéfinies.
env0 :: VEnv
env0 = let binop :: (Int -> Int -> Int) -> Value
           binop op = Vop (\v1 ->
                           case v1 of
                             (Vnum n1)
                              -> return (Vop (\v2 ->
                                              case v2 of
                                                (Vnum n2)
                                                 -> return (Vnum (n1 `op` n2))
                                                _ -> error "Pas un nombre"))
                             _ -> error "Pas un nombre")

          in [("print", Vop (\v -> do hPutStr stdout (show v ++ "\n")
                                      return Vnil)),
              ("+", binop (+)),
              ("*", binop (*)),
              ("/", binop div),
              ("-", binop (-))]

---------------------------------------------------------------------------
-- Évaluateur                                                            --
---------------------------------------------------------------------------

elookup :: VEnv -> Var -> Value
elookup [] x = error ("Variable inconnue: " ++ x)
elookup ((y,v):env) x = if x == y then v else elookup env x

eval :: VEnv -> Lexp -> IO Value
eval _ (Lnum n) = return (Vnum n)
eval env (Lvar x) = return (elookup env x)
eval _ Lnull = return (Vnil)
eval env (Lnode e1 e2) = do v1 <- eval env e1
                            v2 <- eval env e2
                            return (Vcons v1 v2)
-- ¡¡COMPLÉTER: la poursuivre conversion à IO dans le code ci-dessous!!
eval env (Ldo e1 e2)
  = case eval env e1 of
     Vop f -> f (eval env e2)
     Vclosure env' arg body -> eval ((arg, eval env e2) : env') body
     v -> error ("Pas une fonction: " ++ show v)
eval env (Lproc arg body) = Vclosure env arg body
eval env (Ldef defs body)
  = let nenv = map (\(x,e) -> (x, eval nenv e)) defs ++ env
    in eval nenv body
eval env (Lcase e enull x1 x2 enode)
  = case eval env e of
     Vnil -> eval env enull
     Vcons v1 v2 -> eval ((x1, v1) : (x2, v2) : env) enode
     v -> error ("Pas une liste: " ++ show v)
-- eval _ e = error ("Pas implanté: " ++ show e)

---------------------------------------------------------------------------
-- Toplevel                                                              --
---------------------------------------------------------------------------

evalSexp :: Sexp -> IO Value
evalSexp = eval env0 . s2l

evalprintlist :: [Sexp] -> IO ()
evalprintlist [] = return ()
evalprintlist (e : es) =
  do v <- evalSexp e
     hPutStr stdout (show v)
     hPutStr stdout ", "

-- Lit un fichier contenant plusieurs Sexps, les évalues l'une après
-- l'autre, et renvoie la liste des valeurs obtenues.
run :: FilePath -> IO ()
run filename =
    do inputHandle <- openFile filename ReadMode
       hSetEncoding inputHandle utf8
       s <- hGetContents inputHandle
       hPutStr stdout "["
       (let sexps s' = case parse pSexps filename s' of
                         Left _ -> [Ssym "#<parse-error>"]
                         Right es -> es
        in evalprintlist (sexps s))
       hPutStr stdout "]\n"
       hClose inputHandle

sexpOf :: String -> Sexp
sexpOf = read

lexpOf :: String -> Lexp
lexpOf = s2l . sexpOf

valOf :: String -> IO Value
valOf = evalSexp . sexpOf
