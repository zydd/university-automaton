import Data.List (intersect, (\\), intercalate, sort)
import System.IO
import System.Process

type State = String
type Delta = (State, Char, State)

data Automaton = Automaton {states :: [State],
                            alphabet :: [Char],
                            transitions ::[Delta],
                            start :: [State],
                            final :: [State]}

closure :: Automaton -> [State] -> [State]
closure a ss = _closure a [] ss
    where _closure _ _ [] = []
          _closure a v (s:ss) = s : (_closure a (s:v) closure_s) ++ (_closure a (closure_s ++ (s:v)) ss)
              where closure_s = (map third $ filter match $ transitions a) \\ (s:v)
                    match (s1, c1, _) = s == s1 && c1 == '\\'
                    third (_, _, s1) = s1

delta :: Automaton -> [State] -> Char -> [State]
delta a s c = _delta a (closure a s) c
    where _delta a [] c = []
          _delta a s '\\' = closure a s
          _delta a (s:ss) c = closure a (delta_s ++ (delta a ss c))
              where delta_s = map third $ filter match $ transitions a
                    match (s1, c1, _) = s == s1 && c == c1
                    third (_, _, s1) = s1

process :: Automaton -> String -> Bool
process a str = _process a (start a) str
    where _process _ [] _ = False
          _process a ss [] = not $ null $ intersect ss (final a)
          _process a ss (c:cs) = _process a (delta a ss c) cs

genGraphViz :: Automaton -> String
genGraphViz a = "digraph fsm {\n" ++
                "\trankdir=LR;\n" ++
                "\tnode [shape = point, color = white]; " ++ startSymbols ++ ";\n" ++
                "\tnode [shape = doublecircle, color = black]; " ++ finalStates ++ ";\n" ++
                "\tnode [shape = circle];\n" ++
                startStates ++
                stateTransitions ++
                "}\n"
    where startSymbols = "start" ++ (intercalate ", start" (start a))
          finalStates = intercalate ", " (final a)
          startStates = let genStart s = "\tstart" ++ s ++ " -> " ++ s ++ ";\n"
                        in concat $ map genStart (start a)
          stateTransitions = let genTransition (s1, l, s2) = "\t" ++ s1 ++ " -> " ++ s2 ++ " [ label = \"" ++ [if l == '\\' then 'λ' else l] ++ "\" ];\n"
                             in concat $ map genTransition (transitions a)


-- toAfd :: Automaton -> Automaton
-- toAfd a = Automaton [] (alphabet a) (trans $ closure a $ start a) [] []
toAfd a = trans $ start a
    where trans s = deltas s (alphabet a)
          deltas _ [] _ = []
          deltas [] _ _ = []
          deltas s (c:cs) ex = let delta_c = delta a s c
                               in if null delta_c then (deltas s cs ex) else (concat $ sort s, c, delta_c) : (deltas s cs ex)
          third (_, _, s) = s
-- map (\(s0, c, s1) -> (s0, c, concat $ sort s1))

p0i1 = Automaton {states = ["A", "B", "C", "D"],
                  alphabet = ['0', '1'],
                  transitions = [("A", '1', "B"),
                                 ("A", '0', "C"),
                                 ("B", '0', "D"),
                                 ("B", '1', "A"),
                                 ("D", '0', "B"),
                                 ("D", '1', "C"),
                                 ("C", '0', "A"),
                                 ("C", '1', "D")],
                  start = ["AC"],
                  final = ["AD"]}

end1 = Automaton {states = ["A", "B"],
                  alphabet = ['0', '1'],
                  transitions = [("A", '0', "A"),
                                 ("A", '1', "A"),
                                 ("A", '1', "B")],
                  start = ["A"],
                  final = ["B"]}

lcycle = Automaton {states = ["A", "B"],
                    alphabet = ['0', '1'],
                    transitions = [("A", '0', "B"),
                                   ("B", '1', "A"),
                                   ("A", '\\', "B"),
                                   ("B", '\\', "A")],
                    start = ["A"],
                    final = ["A"]}

afn = Automaton {states = ["A", "B", "C", "D", "E"],
                 alphabet = ['0', '1'],
                 transitions = [("A", '0', "B"),
                                ("B", '0', "C"),
                                ("C", '1', "D"),
                                ("D", '1', "C"),
                                ("D", '1', "E")],
                  start = ["A", "B"],
                  final = ["E"]}

-- main = do
--     handle <- openFile "automaton.dot" WriteMode
--     hPutStrLn handle $ genGraphViz end1
--     hClose handle
--     system "dot -Tsvg -O automaton.dot"
--     return ()

main = putStrLn $ show $ toAfd afn