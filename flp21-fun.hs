module Main where
import System.Environment (getArgs)
import Data.List (intercalate)

type Symbol = Char

type Symbols = [Symbol]

type Rule = (Symbol,Symbols)

type Rules = [Rule]

-- datová struktura bezkontextové gramatiky
data BKG = BKG {
    nTerminals :: Symbols,
    terminals :: Symbols,
    startNTerminal :: Symbol,
    rules :: Rules
}

--výpis dle zadani
{-
seznam všech neterminálů
seznam všech terminálů
počáteční neterminál
pravidlo 1
.
.
.
pravidlo n
-}
instance Show BKG where
  show (BKG nTerminals terminals startNTerminal rules) = unlines $
    [intercalate "," [[z]| z <- nTerminals]] ++
    [intercalate "," [[z]| z <- terminals]] ++
    [[startNTerminal]] ++
    map (\(left, right) -> [left] ++ "->" ++ right) rules

-- zajistuje beh programu
-- main vraci strukturu BKG

main :: IO ()
main = do
    args <- getArgs
    let (option, fileName) = parseArs args
    bkg <- gibMyInput fileName
    let new_bkg = decodeArgs option bkg
    print new_bkg
    return()

-- parsuje argumenty funkce a binduje je na promene v main
-- parsovane argumenty jsou zakodovany pomoci cisel pro dalsi dekodaci
parseArs :: Num a => [[Char]] -> (a, [Char])
parseArs [] = error "we are expecting some options. Usage ./flp21-fun.hs argument [input file] more in README"
parseArs [option] = parseArs [option, ""]
parseArs [option,fileName]
    | option == "-i" = (0, fileName)
    | option == "-1" = (1, fileName)
    | option == "-2" = (2, fileName)
    | otherwise = error "unknown argument"
parseArs _ = error "we are expecting 1 or 2 arguments"

-- dokedovani cisel argumentu na funkce
decodeArgs :: (Eq a, Num a) => a -> BKG -> BKG
decodeArgs option bkg
    | option == 0 = bkg
    | option == 1 = firstStep bkg
    | option == 2 = secondStep (firstStep bkg)
    | otherwise = error "Unknown argID"

--funkce na precteni vstupu (soubor nebo STDIN) a naparsovani na BKG
gibMyInput :: [Char] -> IO BKG
gibMyInput fileName = do
    if fileName /= "" then do
        contents <- readFile fileName
        return (parser (lines contents))
    else do
        parser . lines <$> getContents

-- parsovani textoveho vstupu na BKG
parser :: [[Char]] -> BKG
parser (nonTerm:term:start:els) = BKG ns ts (startSymbolParse ns start) (rulesParse ts ns els)
    where
        ns = nonTerminalParse nonTerm
        ts = terminalParse term
parser _ = error "Parsing Error"

-- overeni zda jsou zadany terminaly spravne
terminalParse :: [Char] -> Symbols
terminalParse (x:y)
    | x `elem` ['a'..'z'] ++ ['#'] = x : terminalParse y
    | x == ',' = terminalParse y
    | otherwise = error "terminal symbols are only a-z ++ #"
terminalParse x = x

-- overeni zda jsou zadany neterminaly spravne
nonTerminalParse :: [Char] -> Symbols
nonTerminalParse (x:y)
    | x `elem` ['A'..'Z'] = x : nonTerminalParse y
    | x == ',' = nonTerminalParse y
    | otherwise = error "nonterminal symbols are only A-Z"
nonTerminalParse x = x

-- overeni zda startovaci symbol je z neterminalu
startSymbolParse :: (Foldable t, Eq p) => t p -> [p] -> p
startSymbolParse terms (s:e)
    | s `elem` terms = s
    | otherwise = error "Start Symbol need to be from nonterminals"
startSymbolParse _ _ = error "ERROR with start Symbol"  

--parsovani pravidel (vsech)
rulesParse :: (Foldable t1, Foldable t2) => t2 Symbol -> t1 Symbol -> [[Symbol]] -> [Rule]
rulesParse terms nonTerms (actual:els) = ruleParse terms nonTerms actual : rulesParse terms nonTerms els
rulesParse _ _ _= []

-- prevod pravidla ve tvaru A->aBc na ('A',['a','B','c'])
ruleParse :: (Foldable t1, Foldable t2) => t2 Symbol -> t1 Symbol -> [Symbol] -> Rule
ruleParse terms nonTerms (st:ar1:ar2:els) = ( parse1stNonTerm nonTerms st , checkArrow (ar1:[ar2]) ++ nonTermParse terms nonTerms els ) :: Rule
ruleParse terms nonTerms _ = error "Wrong rule format"

--kontrola zda levá a pravá strana jsou odděleny šipkou (a navíc, že na levé straně je pouze jeden charakter)
checkArrow :: [Char] -> [a]
checkArrow arrow
    | arrow == "->" = []
    | otherwise = error "rules needs to be separated by '->'"

-- kontrola ze na leve strane pravidla je neterminal
parse1stNonTerm :: (Foldable t, Eq p) => t p -> p -> p
parse1stNonTerm nonTerms st
    | st `elem` nonTerms = st
    | otherwise = error "Rules need to have nonterminal on left"

-- kontrola ze prava strana pravidel obsahuje pouze jiz zname terminaly a neterminaly
nonTermParse :: (Eq a, Foldable t1, Foldable t2) => t2 a -> t1 a -> [a] -> [a]
nonTermParse terms nonTerms (act:symbs)
    | act `elem` nonTerms = act : nonTermParse terms nonTerms symbs
    | act `elem` terms = act : nonTermParse terms nonTerms symbs
nonTermParse terms nonTerms act = act

-- provedeni prvniho kroku algorytmu 
--firstStep :: BKG -> BKG
firstStep :: BKG -> BKG
firstStep bkg = BKG completeNTerms (terminals bkg) (startNTerminal bkg) completeRules
    where
        rulesWithoutCycles = detectCycles (nTerminals bkg) (rules bkg) 
        newRules = terminalOnRight (terminals bkg) rulesWithoutCycles
        newNTerms = nTermsFromRules newRules
        (scrabledRules,scrabledNTerms) = resolveRulesAndNTerminals rulesWithoutCycles newRules newNTerms []
        completeRules = sortElements scrabledRules (rules bkg)
        completeNTerms = sortElements scrabledNTerms (nTerminals bkg)

-- odstrani list pravidel z jineho listu pravidel
removeRules :: Eq a => [a] -> [a] -> [a]
removeRules (remove:toRemove) rules = removeRules toRemove newRules
    where
        newRules = removeItemFromList remove rules
removeRules _ rules = rules

-- detekuje a odstrani potencionalni cykly v BKG
detectCycles :: (Foldable t, Eq (t a), Eq a) => [a] -> [(a, t a)] -> [(a, t a)]
detectCycles (nTerm:nTerms) rules
    | detectCycle nTerm nTermRules = detectCycles nTerms (removeRules nTermRules rules)
    | otherwise = detectCycles nTerms rules
    where
        nTermRules = getRulesFromNTerm nTerm rules
detectCycles _ rules = rules

-- ziskani seznam pravidel pro neterminal
getRulesFromNTerm :: Eq t => t -> [(t, b)] -> [(t, b)]
getRulesFromNTerm nTerm (rule:rules)
    | nTerm == getLeftSide rule = rule : getRulesFromNTerm nTerm rules
    | otherwise = getRulesFromNTerm nTerm rules
getRulesFromNTerm _ _ = []

-- zjisti jestli pravidla pro dany neterminal jsou v cyklu nebo ne
detectCycle :: (Foldable t1, Eq t2) => t2 -> [(a, t1 t2)] -> Bool
detectCycle nTerm (rule:rules) = isCycle nTerm rule && detectCycle nTerm rules
detectCycle _ _ = True

-- zjisti zda dane pravidle je cyklus
isCycle :: (Foldable t, Eq a1) => a1 -> (a2, t a1) -> Bool
isCycle nTerm rule
    | nTerm `elem` getRightSide rule = True
    | otherwise = False
    

-- serazeni elementu podle puvodni sekvence
sortElements :: (Foldable t, Eq a) => t a -> [a] -> [a]
sortElements newSet (el:oldSet)
    | el `elem` newSet = el : sortElements newSet oldSet
    | otherwise = sortElements newSet oldSet
sortElements _ _ = []

-- iterace zkrz pravidla a neterminaly (seznam vsech neterminalu, pres ktere se da dostat k terminalum)
resolveRulesAndNTerminals :: [(Char, [Char])] -> [(Char, [Char])] -> [Char] -> [(Char, [Char])] -> ([(Char, [Char])], [Char])
resolveRulesAndNTerminals rules newRules newNTerms explored
    | addedRules == explored = (rulesTwo,newNTermsComplete)
    | otherwise = resolveRulesAndNTerminals rules rulesTwo newNTermsComplete addedRules
    where
        addedRules = terminalOnRight newNTerms rules
        rulesTwo = rulesDupliciySolve (newRules ++ addedRules) []
        newNTermsComplete = noDuplicity (nTermsFromRules rulesTwo) []

-- ziskani pravidel, ktere maji na prave strane terminal
terminalOnRight :: [Char] -> [(a, [Char])] -> [(a, [Char])]
terminalOnRight terms (rule:rules)
    | isTerminals terms (getRightSide rule) = rule : terminalOnRight terms rules
    | otherwise = terminalOnRight terms rules
terminalOnRight terms _ = []

-- ziskani prave strany pravidla
getRightSide :: (a, b) -> b
getRightSide (l,r) = r

-- ziskani leve strany pravidla
getLeftSide :: (a, b) -> a
getLeftSide (l,r) = l

-- detekce terminal ze seznamu Symbolu
isTerminals :: [Char] -> [Char] -> Bool
isTerminals terms (toTest:els)
    | toTest `elem` (terms ++ ['#']) = True
    | otherwise = isTerminals terms els
isTerminals terms _ = False

-- extrakce neterminalu z pravidel
nTermsFromRules :: [(a, b)] -> [a]
nTermsFromRules (rule:rules) = getLeftSide rule : nTermsFromRules rules
nTermsFromRules _ = []

-- provedeni druheho kroku algorytmu

secondStep :: BKG -> BKG
secondStep bkg
    | null (nTerminals bkg) = BKG [startNTerminal bkg] (terminals bkg) (startNTerminal bkg) []
    | otherwise = BKG completeNTerms completeTerms (startNTerminal bkg) completeRules
    where
        newNTerms = accessFromStartTotal [startNTerminal bkg] (nTerminals bkg) (rules bkg) []
        newTerms = noDuplicity (accessibleTerminals newNTerms (terminals bkg) (rules bkg)) []
        redundantNTerms = notMatching (nTerminals bkg) newNTerms
        redundantTerms = notMatching (terminals bkg) newTerms
        newRules = rulesDupliciySolve (reduceRules redundantNTerms (rules bkg)) []
        completeNTerms = sortElements newNTerms (nTerminals bkg)
        completeTerms = sortElements newTerms (terminals bkg)
        completeRules = sortElements newRules (rules bkg)

-- ziskani vsech neterminalu ktere jsou pristupne z nejakeho symbolu
accessFromStartTotal :: (Foldable t1, Eq a) => [a] -> t1 a -> [(a, [a])] -> [a] -> [a]
accessFromStartTotal (start:all) symbs rules explored
    | null allSymbols = start:explored
    | start `elem` explored = accessFromStartTotal (removeItemFromList start discovered) symbs rules explored
    | otherwise = accessFromStartTotal accessible symbs rules exploredNew
        where
            exploredNew = start : explored
            discovered = accessFromStart start symbs rules
            allSymbols = all ++ discovered
            accessible = noDuplicity allSymbols []
accessFromStartTotal _ _ _ explored = explored

-- odstrani prvek ze seznamu
removeItemFromList :: Eq a => a -> [a] -> [a]
removeItemFromList x (y:ys)
    | x == y = removeItemFromList x ys
    | otherwise = y : removeItemFromList x ys
removeItemFromList _ [] = []

-- ziskani seznamu neterminalu, ktere jsou pristupne ze symbolu `start` (naprimo)
accessFromStart :: (Foldable t1, Eq t2, Eq a) => t2 -> t1 a -> [(t2, [a])] -> [a]
accessFromStart start symbs (rule:rules)
    | start == getLeftSide rule = symbsRightSide symbs (getRightSide rule) ++ accessFromStart start symbs rules
    | otherwise = accessFromStart start symbs rules
accessFromStart _ _ _ = []


-- ziskani dostupnych terminalu podle dostupnych neterminalu
accessibleTerminals :: (Foldable t1, Eq t2, Eq a) => [t2] -> t1 a -> [(t2, [a])] -> [a]
accessibleTerminals (nTerm:all) terms rules = accessFromStart nTerm terms rules ++ accessibleTerminals all terms rules
accessibleTerminals _ _ _ = []

-- funkce na vylouceni duplicit ze seznamu
noDuplicity :: Eq a => [a] -> [a] -> [a]
noDuplicity (symb:all) allSymbols
    | symb `elem` allSymbols = noDuplicity all allSymbols
    | otherwise = symb : noDuplicity all (symb:allSymbols)
noDuplicity _ _ = []

--ziskani symbolu na prave strane pravidla, ktere patri do nejake skupiny
symbsRightSide :: (Foldable t, Eq a) => t a -> [a] -> [a]
symbsRightSide symbs (symb:rule)
    | symb `elem` symbs = symb : symbsRightSide symbs rule
    | otherwise = symbsRightSide symbs rule
symbsRightSide _ _ = []

-- rozdil 2 poli
notMatching :: (Foldable t, Eq a) => [a] -> t a -> [a]
notMatching (act:all) toMatch
    | act `elem` toMatch = notMatching all toMatch
    | otherwise = act : notMatching all toMatch
notMatching _ _ = []

-- odstraneni pravidel, ke kterym se neda dostat
reduceRules :: Eq a => [a] -> [(a, b)] -> [(a, b)]
reduceRules (symb:nTerms) rules = reduceRules nTerms reducedRules
    where
        reducedRules = removeNTermRules symb rules
reduceRules _ rules = rules

-- odstraneni konkretniho neterminalu z pravidel
removeNTermRules :: Eq t => t -> [(t, b)] -> [(t, b)]
removeNTermRules nTerm (rule:rules)
    | nTerm /= getLeftSide rule = rule : removeNTermRules nTerm rules
    | otherwise = removeNTermRules nTerm rules
removeNTermRules _ _ = []

-- odstraneni duplicitnich pravidel
rulesDupliciySolve :: Eq a => [a] -> [a] -> [a]
rulesDupliciySolve (rule:rules) newRules
    | rule `elem` newRules = rulesDupliciySolve rules newRules
    | otherwise = rule : rulesDupliciySolve rules (rule:newRules)
rulesDupliciySolve _ _ = []