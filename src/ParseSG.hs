{-|
Module      : ParseSG
Description : This module provides a simple applicative parser
              to import standard SGF recods of games.
              (Functionality is limited to the data structure
              provided by ModelSG, therefore most properties
              as well as variations are discarded.)
              Fundamentals are adopted from the lecture
              "Fortgeschrittene funktionale Programmierung".
-}
module ParseSG
( createSGF
, parseSGF
, earReddeningGame_sgf
, sedolAlphaGo_4_sgf
)

where

import Control.Applicative ( Alternative(..) )
import Data.Char (isUpper, chr, ord)
import Data.Maybe (fromJust, listToMaybe)

import ModelSG


-- | Converts a given game into a string that is conform to the SGF standard.
createSGF :: GameSG -> String
createSGF game = "(" ++ (concat $ map moveToNode game) ++ ")"

moveToNode :: MoveSG -> String
moveToNode (MoveSG color doing) = ";" ++ encodeC color ++ encodeD doing

encodeC :: Color -> String
encodeC Black = "B"
encodeC White = "W"

encodeD :: MoveGo -> String
encodeD Pass = "[]"
encodeD (Play (x,y)) = "[" ++ [chr (x + 96)] ++ [chr (y + 96)] ++ "]"


-- To parse actual SGF files, the SGF data structure must be represented within the parser.
-- However, all properties not needed for the SG application are mapped to stubs and ignored.

type SGF_Collection = [SGF_GameTree]
data SGF_GameTree   = SGF_GameTree SGF_Sequence [SGF_GameTree]
type SGF_Sequence   = [SGF_Node]
type SGF_Node       = [SGF_Property]
data SGF_Property   = IgnoredProperty | Property MoveSG

instance Show SGF_GameTree where
  show (SGF_GameTree sequence subTree) = "(" ++ show sequence ++ show subTree ++ ")"

instance Show SGF_Property where
  show IgnoredProperty = "?"
  show (Property move) = show move


-- | Parses a given string in SGF format and returns the main line of moves.
-- SGF files save game trees in order to represent variations, if there are,
-- and can even hold a collection of independent game trees.
-- Here, we only parse the main line of the first game and discard all other
-- games from the collection and all other moves from the one game in order
-- to obtain a list of coherent moves instead of a tree structure.
parseSGF :: String -> Either String [MoveSG]
parseSGF sgf = mainLine $ parse pCollection' (stripLineBreaks sgf)
  where
    mainLine :: Maybe SGF_Collection -> Either String [MoveSG]
    mainLine Nothing           = Left "Could not parse SGF file."
    mainLine (Just collection) = Right $ mainSequenceFromTree $ head collection


-- | Removes line breaks from a given string to prepare it for the parser.
stripLineBreaks :: String -> String
stripLineBreaks = filter ((/=) '\n')


-- | Returns the main sequence of moves from a given game tree.
mainSequenceFromTree :: SGF_GameTree -> [MoveSG]
mainSequenceFromTree (SGF_GameTree sequence [])     = extractMoves sequence
mainSequenceFromTree (SGF_GameTree sequence (t:ts)) = extractMoves sequence ++ mainSequenceFromTree t


-- | Extracts the moves from a given sequence of SGF properties.
extractMoves :: SGF_Sequence -> [MoveSG]
extractMoves sequence = help sequence []
  where
    help []        acc = acc
    help (node:ns) acc = help ns (if moveFromNode node == Nothing
                                  then acc
                                  else acc ++ [fromJust (moveFromNode node)])


-- | Returns the (one, if any) move from a given node.
moveFromNode :: SGF_Node -> Maybe MoveSG
moveFromNode []                   = Nothing
moveFromNode ((Property move):_)  = Just move
moveFromNode (IgnoredProperty:ps) = moveFromNode ps



runParserTest :: IO ()
runParserTest = do
  rawSgf <- readFile "./sedolAlphaGo1.sgf"
  putStrLn "\nRepresentation of the whole SGF file after first parsing step:\n"
  putStrLn $ show $ parse pCollection' (stripLineBreaks rawSgf)
  putStrLn "\nRepresentation of the main line of moves:\n"
  putStrLn $ show $ parseSGF rawSgf
  return ()


-- Examples:

-- sample SGF string
sampleSGF = "(;FF[4]GM[1]SZ[19];B[aa];W[bb](;B[cc]N[Var A];W[dd];B[ad];W[bd])(;B[hh]N[Var B];W[hg])(;B[gg]N[Var C];W[gh];B[hh](;W[hg]N[Var A];B[kk])(;W[kl]N[Var B])))"

-- Shusaku (Black) versus Gennan Inseki (White), famous for the "ear reddening move"
earReddeningGame_sgf = "(;B[qd];W[dc];B[pq];W[oc];B[cp];W[cf];B[ep];W[qo];B[pe];W[np];B[po];W[pp];B[op];W[qp];B[oq];W[oo];B[pn];W[qq];B[nq];W[on];B[pm];W[om];B[pl];W[mp];B[mq];W[ol];B[pk];W[lq];B[lr];W[kr];B[lp];W[kq];B[qr];W[rr];B[rs];W[mr];B[nr];W[pr];B[ps];W[qs];B[no];W[mo];B[qr];W[rm];B[rl];W[qs];B[lo];W[mn];B[qr];W[qm];B[or];W[ql];B[qj];W[rj];B[ri];W[rk];B[ln];W[mm];B[qi];W[rq];B[jn];W[ls];B[ns];W[gq];B[go];W[ck];B[kc];W[ic];B[pc];W[nj];B[ke];W[og];B[oh];W[pb];B[qb];W[ng];B[mi];W[mj];B[nd];W[ph];B[qg];W[pg];B[hq];W[hr];B[ir];W[iq];B[hp];W[jr];B[fc];W[lc];B[ld];W[mc];B[lb];W[mb];B[md];W[qf];B[pf];W[qh];B[rg];W[rh];B[sh];W[rf];B[sg];W[pj];B[pi];W[oi];B[oj];W[ni];B[qk];W[ok];B[qe];W[kb];B[jb];W[ka];B[jc];W[ob];B[ja];W[la];B[db];W[cc];B[fe];W[cn];B[gr];W[is];B[fq];W[io];B[ji];W[eb];B[fb];W[eg];B[dj];W[dk];B[ej];W[cj];B[dh];W[ij];B[hm];W[gj];B[eh];W[fl];B[fg];W[er];B[dm];W[fn];B[dn];W[gn];B[jj];W[jk];B[kk];W[ii];B[ik];W[jl];B[kl];W[il];B[jh];W[co];B[do];W[ih];B[hn];W[hl];B[bl];W[dg];B[gh];W[ch];B[ig];W[ec];B[cr];W[fd];B[gd];W[ed];B[gc];W[bk];B[cm];W[gs];B[gp];W[li];B[kg];W[in];B[lj];W[lg];B[gm];W[jf];B[jg];W[im];B[fm];W[kf];B[lf];W[mf];B[le];W[gf];B[hf];W[ff];B[gg];W[lk];B[kj];W[km];B[lm];W[ll];B[jm];W[ge];B[he];W[ef];B[ea];W[cb];B[fr];W[fs];B[dr];W[qa];B[ra];W[pa];B[rb];W[da];B[gi];W[fj];B[fi];W[fa];B[ga];W[gl];B[ek];W[em];B[ho];W[el];B[en];W[jo];B[kn];W[ci];B[lh];W[mh];B[mg];W[di];B[ei];W[lg];B[qn];W[rn];B[re];W[sl];B[mg];W[bm];B[am];W[lg];B[eq];W[es];B[mg];W[ha];B[gb];W[lg];B[ds];W[hs];B[mg];W[sj];B[si];W[lg];B[sr];W[sq];B[mg];W[hd];B[hb];W[lg];B[ro];W[so];B[mg];W[ss];B[qs];W[lg];B[sn];W[rp];B[mg];W[cl];B[bn];W[lg];B[ml];W[mk];B[mg];W[pj];B[sf];W[lg];B[nn];W[nl];B[mg];W[ib];B[ia];W[lg];B[nc];W[nb];B[mg];W[jd];B[kd];W[lg];B[ma];W[na];B[mg];W[qc];B[rc];W[lg];B[js];W[ks];B[mg];W[hc];B[id];W[lg];B[fk];W[hj];B[mg];W[hh];B[hg];W[lg];B[gk];W[hk];B[mg];W[ak];B[lg];W[al];B[bm];W[nf];B[od];W[ki];B[ms];W[kp];B[ip];W[jp];B[lr];W[oj];B[mr];W[ea];B[sr])"

-- Fourth game between Lee Sedol (White) and AlphaGo (Black)
sedolAlphaGo_4_sgf = "(;GM[1]FF[4]CA[UTF-8]AP[CGoban:3]ST[2]RU[Chinese]SZ[19]KM[7.50]TM[7200]OT[3x60 byo-yomi]PW[Lee Sedol ]PB[AlphaGo]WR[9d]DT[2016-03-13]EV[Google DeepMind Challenge Match]RO[Game 4]PC[Seoul, Korea]WT[Human]BT[Computer]SO[https://gogameguru.com/]RE[W+Resign];B[pd];W[dp];B[cd];W[qp];B[op];W[oq];B[nq];W[pq];B[cn];W[fq];B[mp];W[po];B[iq];W[ec];B[hd];W[cg];B[ed];W[cj];B[dc];W[bp];B[nc];W[qi];B[ep];W[eo];B[dk];W[fp];B[ck];W[dj];B[ej];W[ei];B[fi];W[eh];B[fh];W[bj];B[fk];W[fg];B[gg];W[ff];B[gf];W[mc];B[md];W[lc];B[nb];W[id];B[hc];W[jg];B[pj];W[pi];B[oj];W[oi];B[ni];W[nh];B[mh];W[ng];B[mg];W[mi];B[nj];W[mf];B[li];W[ne];B[nd];W[mj];B[lf];W[mk];B[me];W[nf];B[lh];W[qj];B[kk];W[ik];B[ji];W[gh];B[hj];W[ge];B[he];W[fd];B[fc];W[ki];B[jj];W[lj];B[kh];W[jh];B[ml];W[nk];B[ol];W[ok];B[pk];W[pl];B[qk];W[nl];B[kj];W[ii];B[rk];W[om];B[pg];W[ql];B[cp];W[co];B[oe];W[rl];B[sk];W[rj];B[hg];W[ij];B[km];W[gi];B[fj];W[jl];B[kl];W[gl];B[fl];W[gm];B[ch];W[ee];B[eb];W[bg];B[dg];W[eg];B[en];W[fo];B[df];W[dh];B[im];W[hk];B[bn];W[if];B[gd];W[fe];B[hf];W[ih];B[bh];W[ci];B[ho];W[go];B[or];W[rg];B[dn];W[cq];B[pr];W[qr];B[rf];W[qg];B[qf];W[jc];B[gr];W[sf];B[se];W[sg];B[rd];W[bl];B[bk];W[ak];B[cl];W[hn];B[in];W[hp];B[fr];W[er];B[es];W[ds];B[ah];W[ai];B[kd];W[ie];B[kc];W[kb];B[gk];W[ib];B[qh];W[rh];B[qs];W[rs];B[oh];W[sl];B[of];W[sj];B[ni];W[nj];B[oo];W[jp]C[https://gogameguru.com/])"



-- Parsers, composed according to the SGF structure

pCollection' :: Parser SGF_Collection
pCollection' = some pTree

pTree :: Parser SGF_GameTree
pTree = one roundBrLeft *> (SGF_GameTree <$> pSequence <*> many pTree) <* one roundBrRight

pSequence :: Parser SGF_Sequence
pSequence = some pNode

pNode :: Parser SGF_Node
pNode = one semikolon *> some pProperty

pProperty :: Parser SGF_Property
pProperty = readMove <|> ignoreIt
  where
    readMove = Property <$> pMove
    ignoreIt = (\_ -> IgnoredProperty) <$> pIgnoredProperty

pMove :: Parser MoveSG
pMove = MoveSG <$> pColor <*> pDoing

pColor :: Parser Color
pColor = decode <$> player
  where
    decode 'B' = Black
    decode 'W' = White

pDoing :: Parser MoveGo
pDoing = pass <|> play
  where
    pass = (\_     -> Pass)       <$> (squareBrLeft *> squareBrRight)
    play = (\point -> Play point) <$> (squareBrLeft *> pPoint <* squareBrRight)

pPoint :: Parser Point
pPoint = (,) <$> pCoordinate <*> pCoordinate

pCoordinate :: Parser Int
pCoordinate = numericValue <$> coordinate
  where
    numericValue c = ord c - 96

pIgnoredProperty :: Parser String
pIgnoredProperty = oneOrTwo upperCase *> pPropValue

pPropValue :: Parser String
pPropValue = (++) <$> one squareBrLeft <*>
            ((++) <$> many legalChar <*>
                      one squareBrRight)



-- little helpers, more often used in a previous version, but still useful
one :: Applicative f => f a -> f [a]
one v = (:) <$> v <*> pure []

two :: Applicative f => f a -> f [a]
two v = (:) <$> v <*> one v

oneOrTwo :: Alternative f => f a -> f [a]
oneOrTwo v = one v <|> two v


-- What follows, is mainly taken from the lecture
-- "Fortgeschrittene funktionale Programmierung",
-- just some basic parsers for chars were added.


-- newtype Parser needed for declaration of instances
newtype Parser a = Parser (String -> [(a,String)])

runParser :: Parser a -> String -> [(a,String)]
runParser (Parser p) s = p s

-- | Returns all possible parsing results
-- which base on the complete input string.
runParserComplete :: Parser a -> String -> [a]
runParserComplete (Parser p) s = [ r | (r,"") <- p s ]

-- | Returns the first complete parse.
parse :: Parser a -> String -> Maybe a
parse p s = listToMaybe $ runParserComplete p s

instance Functor Parser where
  fmap f (Parser p) = Parser $ \s -> map (\(a,b) -> (f a, b)) $ p s
  
instance Applicative Parser where
  -- konsumiert keine Eingabe und liefert immer ein Ergebnis
  -- pure :: a -> Parser a
  pure x = Parser $ \s -> [(x,s)]
  
  -- parsed eine Funktion und aus dem Rest der Eingabe ein Argument für diese Funktion und liefert das Ergebnis
  -- <*> :: Parser (a -> b) -> Parser a -> Parser b
  (Parser p1) <*> (Parser p2) = Parser $ \inp ->
    [(r1 r2, rem2) | (r1,rem1) <- p1 inp, (r2,rem2) <- p2 rem1]

-- Ebenfalls in Modul Control.Applicative definiert:
-- Typklasse Alternative ist eine Unterklasse für Applikative Funktoren 
-- mit Monoid-Struktur, d.h.: es gibt eine assoziative binäre Verknüpfung mit neutralem Element!
instance Alternative Parser where
  -- neutrales Element, ein Parser der immer fehlschlägt
  -- empty :: Parser a
  empty = Parser $ \s -> []
  
  -- verknüpft zwei Parser zu einem Parser, welcher beides alternativ parsen kann
  -- <|> :: Parser a -> Parser a -> Parser a
  (Parser p1) <|> (Parser p2) = Parser pbranches
    where
      pbranches s 
        | null r1   = r2
        | null r2   = r1
        | otherwise = r1 ++ r2        
        where
          r1 = p1 s
          r2 = p2 s

-- Basic Parser creating funtion          
satisfy :: (Char -> Bool) -> Parser Char -- parse a desired character
satisfy p = Parser check
  where 
    check (c:s) | p c = [(c,s)] -- successful
    check   _         = [     ] -- no parse


-- Some basic parsers for chars, used to build up the more complex one above.
legalChar :: Parser Char
legalChar = satisfy $ \c -> c /= ']'

semikolon :: Parser Char
semikolon = satisfy $ \c -> c == ';'

upperCase :: Parser Char
upperCase = satisfy isUpper

player :: Parser Char
player = satisfy $ \c -> c == 'B' || c == 'W'

coordinate :: Parser Char
coordinate = satisfy (`elem` ['a'..'s'])

roundBrLeft :: Parser Char
roundBrLeft = satisfy (== '(')

roundBrRight :: Parser Char
roundBrRight = satisfy (== ')')

squareBrLeft :: Parser Char
squareBrLeft = satisfy (== '[')

squareBrRight :: Parser Char
squareBrRight = satisfy (== ']')
