

module Logic where


-- Evaluation
import Data.Char
import Data.List

type SentenceLetter = String
type Value = Bool

data Operator = And | Or | If | Iff deriving Show


data Sentence = Variable SentenceLetter | Negate Sentence | Apply Operator Sentence Sentence deriving Show


type Interpretation = [(SentenceLetter, Value)]

type TruthTable = [(Interpretation, Value)]

untabs = intercalate "\t"

printTruthTable :: TruthTable -> String
printTruthTable is = (headers is) ++ "\n" ++ (concat (map showLine is)) where
	showLine :: (Interpretation, Value) -> String
	showLine (i, v) = (untabs (map (show.snd) i)) ++ "\t" ++ (show v) ++ "\n"
	headers :: TruthTable -> String
	headers ((i, _):ivs) = untabs (map fst i)


evaluate :: Interpretation -> Sentence -> Value
evaluate i (Variable s) = getvalue i s
evaluate i (Negate phi) = not $ evaluate i phi
evaluate i (Apply operator a b) = apply operator (evaluate i a) (evaluate i b)

apply :: Operator -> Value -> Value -> Value
apply And a b = a && b
apply Or a b = a || b
apply If a b = (not a) || (b)
apply Iff a b = (apply If a b) && (apply If b a)

getvalue :: Interpretation -> SentenceLetter -> Value
getvalue i s = if values == [] then error "Undefined SentenceLetter ++ " s else head values
	where values = [v | (s2, v) <- i, s==s2]

putvalue :: SentenceLetter -> Value -> Interpretation -> Interpretation
putvalue s v i = (s, v):(filter ((/=s).fst) i)


-- Parsing, (value matched, rest of string)

type Parse a = String -> Maybe (a, String)

token :: Char -> Parse Char
token c (x:xs) = if x==c then Just (c,xs) else Nothing
token c [] = Nothing

spot :: (Char -> Bool) -> Parse Char
spot p (x:xs) = if p x then Just (x,xs) else Nothing
spot p [] = Nothing
-- Functions for Combining Parsers


-- Chain Parsers if their outputs are sucessful

infixr 5 ^^^
(^^^) :: Parse a -> Parse b -> Parse (a,b)

(p1 ^^^ p2) inp =
	case p1 inp of
		Nothing -> Nothing
		Just (a, p1out) -> case p2 p1out of
			Nothing -> Nothing
			Just (b, p2out) -> Just ((a,b), p2out)

-- Apply a function to the output of a parser

infixl 4 >>>
(>>>) :: Parse a -> (a -> b) -> Parse b

(p1 >>> f) inp =
	case p1 inp of
		Nothing -> Nothing
		Just (a, p1out) -> Just (f a, p1out)

-- Apply the first parser which succeeds (or Nothing)

infixr 3 |||
(|||) :: Parse a -> Parse a -> Parse a

(p1 ||| p2) inp =
	case p1 inp of
		Just (a, p1out) -> Just (a, p1out)
		Nothing -> p2 inp

-- Apply a parser as many times as possible, accumalating the results into a list

many :: Parse a -> Parse [a]
many p =
	p ^^^ many p >>> join
	|||
	\inp -> Just ([], inp)
	where join (a,b) = a:b

-- Parsing L1

-- Note: L1 SentenceLetters are capital letters followed by optional digits
parseLetter :: Parse SentenceLetter
parseLetter =
	spot isUpper ^^^ many (spot isDigit) >>> join
	where join (a,b) = a:b

parseOperator :: Parse Operator
parseOperator = spot (`elem` "&v>=") >>> buildOperator
	where
		buildOperator '&' = And
		buildOperator 'v' = Or
		buildOperator '>' = If
		buildOperator '=' = Iff

parseBinary :: Parse Sentence
parseBinary =
	parseSentence ^^^ parseOperator ^^^ parseSentence >>> buildSentence
	where
		buildSentence :: (Sentence, (Operator, Sentence)) -> Sentence
		buildSentence (s1, (op, s2)) = Apply op s1 s2

parseSentence :: Parse Sentence
parseSentence =
	token '(' ^^^ parseBinary ^^^ token ')' >>> remPars
	|||
	token '~' ^^^ parseSentence >>> remPre >>> Negate
	|||
	parseLetter >>> Variable
	where
		remPars (c1, (e, c2)) = e
		remPre (_, e) = e

parseInput :: Parse a -> String -> a
parseInput p inp =
	case p inp of
		Just (result,"") -> result
		Just (result,rest) -> error ("parse failed; unconsumed input: " ++ rest)
		Nothing -> error "parse unsucessful"

parseL1 :: String -> Sentence
parseL1 = parseInput parseSentence

evalL1 :: Interpretation -> String -> Value
evalL1 i = evaluate i . parseL1

truthTables :: Sentence -> [(Interpretation, Value)]
truthTables s = [(i, evaluate i s) | i <- (genInterpretations (getLetters s))]

genInterpretations :: [SentenceLetter] -> [Interpretation]
genInterpretations [] = [[]]
genInterpretations (s:ss) = (map (putvalue s True) is) ++ (map (putvalue s False) is) where is = genInterpretations ss

getLetters :: Sentence -> [SentenceLetter]
getLetters (Variable sl) = [sl]
getLetters (Negate s) = getLetters s
getLetters (Apply _ s1 s2) = union (getLetters s1) (getLetters s2)
