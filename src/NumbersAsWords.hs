module NumbersAsWords where

data Molecule = Molecule
  { magnitude :: Integer
  , label :: String
  }

molecules =
  [ Molecule 1000000000000 "Trillion"
  , Molecule 1000000000 "Billion"
  , Molecule 1000000 "Million"
  , Molecule 1000 "Thousand"
  , Molecule 100 "Hundered"
  , Molecule 90 "Ninety"
  , Molecule 80 "Eighty"
  , Molecule 70 "Seventy"
  , Molecule 60 "Sixty"
  , Molecule 50 "Fifty"
  , Molecule 40 "Forty"
  , Molecule 30 "Thirty"
  , Molecule 20 "Twenty"
  ]

atomify :: Integer -> Integer -> (Maybe Integer, Maybe Integer)
atomify number mag =
  let mol = rem number mag
      remainder =
        if mol == 0
          then Nothing
          else Just mol
      quotient =
        if number < 100
          then Nothing
          else Just (quot number mag)
  in (quotient, remainder)

findFirst :: Integer -> [Molecule] -> Molecule
findFirst num (c:cs) =
  if magnitude c <= num
    then c
    else findFirst num cs

recNumAsWords (Just num) = numberAsWords num
recNumAsWords Nothing = []

numberAsWords :: Integer -> [String]
numberAsWords number =
  case number of
    0 -> ["Zero"]
    1 -> ["One"]
    2 -> ["Two"]
    3 -> ["Three"]
    4 -> ["Four"]
    5 -> ["Five"]
    6 -> ["Six"]
    7 -> ["Seven"]
    8 -> ["Eight"]
    9 -> ["Nine"]
    10 -> ["Ten"]
    11 -> ["Eleven"]
    12 -> ["Twelve"]
    13 -> ["Thirteen"]
    14 -> ["Fourteen"]
    15 -> ["Fifteen"]
    16 -> ["Sixteen"]
    17 -> ["Seventeen"]
    18 -> ["Eighteen"]
    19 -> ["Nineteen"]
    _ ->
      let Molecule mag label = findFirst number molecules
          (count, rest) = atomify number mag
      in recNumAsWords count ++ [label] ++ recNumAsWords rest


