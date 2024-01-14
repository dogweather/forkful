---
title:    "Haskell: Konwertowanie daty na ciąg znaków"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Konwersja daty na ciąg znaków jest częstym zadaniem w programowaniu. W tym artykule dowiesz się jak to zrobić w języku Haskell.

## Jak To Zrobić

```Haskell
import Data.Time

convertDateToString :: UTCTime -> String
convertDateToString date = formatTime defaultTimeLocale "%Y-%m-%d" date

main = do
    let date = fromGregorian 2021 10 31
    let stringDate = convertDateToString date
    putStrLn stringDate
```

### Przykładowy wynik: "2021-10-31"

W powyższym kodzie najpierw musimy zaimportować moduł Data.Time, który zawiera funkcje do obsługi daty i czasu w Haskellu. Następnie definiujemy funkcję convertDateToString, która przyjmuje jako argument datę w formacie UTCTime i zwraca ją w postaci ciągu znaków w formacie "rok-miesiąc-dzień".

W funkcji main tworzymy zmienną date, która będzie datą, którą chcemy przekonwertować. Następnie używamy funkcji convertDateToString, aby przekonwertować tę datę na string i wyświetlamy go za pomocą funkcji putStrLn.

## Głębsze Wprowadzenie

W powyższym przykładzie użyliśmy formatu "%Y-%m-%d", jednak istnieje wiele innych możliwości formatowania daty za pomocą funkcji formatTime. Na przykład, możemy dodać informacje o godzinie i minucie, używając formatu "%Y-%m-%d %H:%M".

Możemy również użyć funkcji getCurrentTime z modułu Data.Time, aby pobrać bieżącą datę i czas, a następnie przekonwertować go na string za pomocą naszej funkcji convertDateToString.

W przeciwieństwie do innych języków programowania, w Haskellu nie musimy martwić się o różnice w formatach daty w zależności od lokalizacji użytkownika. Dzięki funkcji defaultTimeLocale, w dowolnym miejscu na świecie nasz string zawsze będzie miał taki sam format.

## Zobacz Również

- [Dokumentacja Data.Time](https://hackage.haskell.org/package/time/docs/Data-Time.html)
- [Porównanie różnych funkcji konwertujących datę na string w Haskellu](https://stackoverflow.com/questions/22333351/converting-date-string-to-a-specific-format-in-haskell)