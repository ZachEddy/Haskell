import Data.List
import qualified Data.Map as Map

main = do
	putStrLn "Word length?"
	wordLength <- getLine
	putStrLn "Number of guesses?"
	numGuesses <- getLine
	let 
		startingDictionary = mediumDict
		filteredDictionary = (filter (\entries -> show (length entries) == wordLength) startingDictionary)
		startingKey = [ '-' | x <- [1..((read wordLength)::Integer)]]
		startingMap = Map.singleton startingKey filteredDictionary
	playGame ((read numGuesses)::Integer) "" startingMap

playGame 0 guesses m = do
	putStrLn "Game over! You ran out of guesses."
playGame n guesses m = do
	putStrLn ""
	putStrLn ("You have " ++ (show n) ++ " guesses left.")
	let mapMax = findMapMax m
	putStrLn ("Letters guessed: " ++ guesses) 
	putStrLn ("Word: " ++ (fst mapMax))
	putStrLn "Guess a letter: "
	guessLine <- getLine
	let nextGuess = head guessLine
	let 
		updatedGuesses = nextGuess : guesses
		guessedMap = patterns updatedGuesses (snd mapMax)
		guessedMapMax = findMapMax guessedMap
	printMap (Map.keys guessedMap) guessedMap
	putStrLn ("Using pattern " ++ (show (fst guessedMapMax)) ++ " which matches " ++ (show (snd (guessedMapMax))))
	if (elem nextGuess (fst guessedMapMax))
		then do
			if not (elem '-' (fst guessedMapMax)) then putStrLn ("You guessed it! The word was " ++ (show (head (snd guessedMapMax))))  else do 
				putStrLn ("Good guess!")
				playGame n updatedGuesses guessedMap
		else do 
			putStrLn ("Sorry, there are no " ++ [nextGuess] ++ "'s")
			playGame (n-1) updatedGuesses guessedMap

--These two, when used together, generate a map using the pattern (key function) and the word itself
patterns guesses dict = Map.fromListWith (++) (map (\word -> ((key word guesses), [word])) dict)
key word guesses = map (\letter -> if (elem letter guesses) then letter else '-') word

--Gets the value out of Just. Pattern matching would've also worked - but I think this is a little cleaner
getJustValue (Just a) = a

--This prints out the map with the given keys
printMap keyList m = do
	putStrLn (key ++ " matches " ++ show (getJustValue (Map.lookup key m)))
	if (tail keyList) == [] then do putStrLn "" else printMap (tail keyList) m
		where key = head keyList 

--Finds the largest list of words in the dictionary and the key that's associated with it
findMapMax m = foldr1 (\(key, value) acc -> if length value > length (snd acc) then (key, value) else acc) keyValuePairs
	where keyValuePairs = map (\key -> (key, getJustValue (Map.lookup key m))) (Map.keys m)

trivialDict = ["ally", "beta", "cool", "deal", "else", "flew", "good", "hope", "ibex"]

smallDict = ["alae", "alee", "ales", "area", "ares", "arse", "asea", "ates", "earl", "ears", "ease", "east", "eats", "eras", "etas", "lase", "late", "leal", "lear", "leas", "rale", "rare", "rase", "rate", "real", "rear", "sale", "sate", "seal", "sear", "seas", "seat", "sera", "seta", "tael", "tale", "tare", "tate", "teal", "tear", "teas", "tela"]

mediumDict = ["abbe", "abed", "abet", "able", "abye", "aced", "aces", "ache", "acme", "acne", "acre", "adze", "aeon", "aero", "aery", "aged", "agee", "ager", "ages", "ague", "ahem", "aide", "ajee", "akee", "alae", "alec", "alee", "alef", "ales", "alme", "aloe", "amen", "amie", "anes", "anew", "ante", "aped", "aper", "apes", "apex", "apse", "area", "ares", "arse", "asea", "ates", "aver", "aves", "awed", "awee", "awes", "axed", "axel", "axes", "axle", "ayes", "babe", "bade", "bake", "bale", "bane", "bare", "base", "bate", "bead", "beak", "beam", "bean", "bear", "beat", "beau", "bema", "beta", "blae", "brae", "cade", "cafe", "cage", "cake", "came", "cane", "cape", "care", "case", "cate", "cave", "ceca", "dace", "dale", "dame", "dare", "date", "daze", "dead", "deaf", "deal", "dean", "dear", "deva", "each", "earl", "earn", "ears", "ease", "east", "easy", "eath", "eats", "eaux", "eave", "egad", "egal", "elan", "epha", "eras", "etas", "etna", "exam", "eyas", "eyra", "face", "fade", "fake", "fame", "fane", "fare", "fate", "faze", "feal", "fear", "feat", "feta", "flea", "frae", "gaed", "gaen", "gaes", "gage", "gale", "game", "gane", "gape", "gate", "gave", "gaze", "gear", "geta", "hade", "haed", "haem", "haen", "haes", "haet", "hake", "hale", "hame", "hare", "hate", "have", "haze", "head", "heal", "heap", "hear", "heat", "idea", "ilea", "jade", "jake", "jane", "jape", "jean", "kaes", "kale", "kame", "kane", "keas", "lace", "lade", "lake", "lame", "lane", "lase", "late", "lave", "laze", "lead", "leaf", "leak", "leal", "lean", "leap", "lear", "leas", "leva", "mabe", "mace", "made", "maes", "mage", "make", "male", "mane", "mare", "mate", "maze", "mead", "meal", "mean", "meat", "mesa", "meta", "nabe", "name", "nape", "nave", "neap", "near", "neat", "nema", "odea", "olea", "pace", "page", "pale", "pane", "pare", "pase", "pate", "pave", "peag", "peak", "peal", "pean", "pear", "peas", "peat", "plea", "race", "rage", "rake", "rale", "rape", "rare", "rase", "rate", "rave", "raze", "read", "real", "ream", "reap", "rear", "rhea", "sabe", "sade", "safe", "sage", "sake", "sale", "same", "sane", "sate", "save", "seal", "seam", "sear", "seas", "seat", "sera", "seta", "shea", "spae", "tace", "tael", "take", "tale", "tame", "tape", "tare", "tate", "teak", "teal", "team", "tear", "teas", "teat", "tela", "tepa", "thae", "toea", "twae", "urea", "uvea", "vale", "vane", "vase", "veal", "vela", "vena", "vera", "wade", "waes", "wage", "wake", "wale", "wame", "wane", "ware", "wave", "weak", "weal", "wean", "wear", "weka", "yare", "yeah", "yean", "year", "yeas", "zeal", "zeta", "zoea"]
