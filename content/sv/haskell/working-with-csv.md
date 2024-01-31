---
title:                "Arbeta med csv"
date:                  2024-01-19
simple_title:         "Arbeta med csv"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why?
CSV, Comma Separated Values, är en enkel textbaserad filstandard för att lagra tabulär data. Programmerare använder CSV för dess enkelhet och kompatibilitet med kalkylprogram och databaser.

## How to:
Du kan hantera CSV-filer i Haskell med pacakget `cassava`. Här är exempel:

```Haskell
{-# LANGUAGE OverloadedStrings #-}
import Data.Csv
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V

-- Om vi har följande CSV:
-- namn,ålder
-- Alice,42
-- Bob,29

main :: IO ()
main = do
    csvData <- BL.readFile "exempel.csv"
    case decode NoHeader csvData of
        Left err -> putStrLn err
        Right v -> V.forM_ v $ \ (name, age) ->
            putStrLn $ name ++ " är " ++ show age ++ " år gammal"
```

Resultat:
```
Alice är 42 år gammal
Bob är 29 år gammal
```

## Deep Dive
CSV är etablerat på 70-talet och är fortfarande populärt för sin enkelhet. Alternativ som JSON eller XML erbjuder mer komplexitet och struktur. Haskell's CSV behandling kan bli djupare med `cassava`'s `decode` och typanpassning, som hanterar läsning och parning.

## See Also
Mer om CSV i Haskell:

- `cassava` dokumentation: [http://hackage.haskell.org/package/cassava](http://hackage.haskell.org/package/cassava)

Andra relevanta källor:

- CSV på Wikipedia: [https://sv.wikipedia.org/wiki/CSV](https://sv.wikipedia.org/wiki/CSV)
- Haskell's hemsida: [https://www.haskell.org/](https://www.haskell.org/)
