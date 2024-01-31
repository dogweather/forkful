---
title:                "Praca z plikami CSV"
date:                  2024-01-19
simple_title:         "Praca z plikami CSV"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/working-with-csv.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?
Praca z plikami CSV (Comma-Separated Values) to częste zadanie — zarządzamy danymi. W Haskellu robimy to, by łatwo przetwarzać i analizować duże zbiory danych w formacie, który jest czytelny dla człowieka i maszyny.

## Jak to zrobić:
W Haskellu do obsługi CSV używamy biblioteki `cassava`. Oto prosty przykład:

```Haskell
import Data.Csv
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V

-- Zakładamy, że mamy plik `przykladowe_dane.csv' z danymi:
-- imie,nazwisko,wiek
-- Jan,Kowalski,34
-- Anna,Nowak,28

main :: IO ()
main = do
    csvData <- BL.readFile "przykladowe_dane.csv"
    case decode NoHeader csvData of
        Left err -> putStrLn err
        Right v -> V.forM_ v $ \(name, surname, age) -> 
            putStrLn $ name ++ " " ++ surname ++ ", wiek: " ++ show age
```

Przykładowe wyjście:
```
Jan Kowalski, wiek: 34
Anna Nowak, wiek: 28
```

## Głębsze spojrzenie:
Historia formatu CSV sięga wczesnych lat informatyki — prostota i elastyczność to powody jego popularności. Alternatywy, jak JSON czy XML, oferują więcej funkcji, ale są mniej praktyczne dla prostych danych tabelarycznych. W Haskellu `Data.Csv` przetwarza CSV do i z wektorów; można też użyć strumieniowania dla dużej wydajności.

## Zobacz też:
- [Hackage `cassava` package](https://hackage.haskell.org/package/cassava)
- [HaskellWiki on CSV](https://wiki.haskell.org/CSV)
