---
title:                "Otrzymywanie bieżącej daty"
html_title:           "Haskell: Otrzymywanie bieżącej daty"
simple_title:         "Otrzymywanie bieżącej daty"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
W programowaniu, pobieranie aktualnej daty jest często wykorzystywaną funkcjonalnością. Pozwala ona na uzyskanie informacji o bieżącym dniu, miesiącu i roku. Programiści korzystają z tej funkcji, aby np. śledzić postępy projektu, tworzyć harmonogramy lub weryfikować ważne terminy.

## Jak to zrobić:
Poniżej znajdują się przykładowe kody napisane w języku Haskell, które pozwolą Ci pobrać aktualną datę i wyświetlić ją w różnych formatach.

```Haskell
-- Zwykłe wyświetlenie aktualnej daty w formacie "YYYY-MM-DD"
import Data.Time
main = do
   dzisiejszaData <- getCurrentDate
   putStrLn $ formatTime defaultTimeLocale "%Y-%m-%d" dzisiejszaData
```
Wynik:
```Haskell
2019-09-18
```

```Haskell
-- Wyświetlenie bieżącego roku i dnia roku
import Data.Time
main = do
   dzisiejszaData <- getCurrentDate
   let rok = formatTime defaultTimeLocale "%Y" dzisiejszaData
       dzienRoku = formatTime defaultTimeLocale "%j" dzisiejszaData
   putStrLn ("Rok: " ++ rok ++ ", Dzień roku: " ++ dzienRoku)
```
Wynik:
```Haskell
Rok: 2019, Dzień roku: 261
```

## Podgląd:
Haskell jest obecnie jednym z najpopularniejszych języków programowania wykorzystujących typy statyczne. Podobne funkcje można znaleźć również w innych językach, takich jak Java, Python czy C++. Ważne jest jednak, aby zwrócić uwagę na różnice w sposób wywoływania tych funkcji oraz w formatowaniu wyników.

## Zobacz też:
Dowiedz się więcej o funkcjonalności pobierania aktualnej daty w Haskell, odwiedzając następujące strony: 

[Documentation for Data.Time package] (https://hackage.haskell.org/package/time-1.9.1/docs/Data-Time.html) 

[Tutorial on using dates and times in Haskell] (https://wiki.haskell.org/Haskell_date_formatting