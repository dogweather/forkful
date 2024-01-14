---
title:    "Haskell: Konwersja daty na ciąg znaków"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Dlaczego

Konwersja daty na ciąg znaków może być niezbędnym zadaniem w wielu projektach programistycznych. Na przykład, jeśli tworzysz aplikację, która wymaga wyświetlania dat w różnych formatach lub do bazy danych, konwersja daty na ciąg znaków jest niezbędnym krokiem.

## Jak to zrobić

Istnieje wiele sposobów na przekonwertowanie daty na ciąg znaków w języku Haskell. Możesz użyć funkcji `show` lub `formatTime` z biblioteki `Data.Time.Format` lub `parseTimeM` z biblioteki `Data.Time.Parse` w zależności od Twoich potrzeb. Poniżej przedstawiamy przykładowy kod oraz wyjście dla każdego z tych rozwiązań.

```Haskell
import Data.Time.Format
import Data.Time.Parse

-- Użycie funkcji show
show (UTCTime (fromGregorian 2021 10 13) (secondsToDiffTime 3720))
-- Wynik: "2021-10-13 01:02:00 UTC"

-- Użycie funkcji formatTime
formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S %Z" (UTCTime (fromGregorian 2021 10 13) (secondsToDiffTime 3720))
-- Wynik: "2021-10-13 01:02:00 UTC"

-- Użycie funkcji parseTimeM
parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S %Z" "2021-10-13 01:02:00 UTC" :: Maybe UTCTime
-- Wynik: Just 2021-10-13 01:02:00 UTC
```

Jak widać, wszystkie trzy metody dają ten sam wynik. Jedyna różnica to sposób ich użycia. Funkcja `show` zwraca po prostu ciąg znaków, a funkcje `formatTime` i `parseTime` wymagają podania formatu daty i strefy czasowej. Możesz eksperymentować z różnymi formatami i strefami czasowymi, aby dostosować wynik do swoich potrzeb.

## Głębszy przegląd

Jeśli interesuje Cię bardziej zaawansowana konwersja daty, możesz zajrzeć do dokumentacji biblioteki `Data.Time` lub poszukać innych rozwiązań dostępnych dla języka Haskell. Istnieją również biblioteki do konwersji daty na ciąg znaków w konkretnych formatach, na przykład `Data.Time.Format.ISO8601`. Możesz również stworzyć własną funkcję konwertującą datę na ciąg znaków według swoich potrzeb.

## Zobacz także

- Dokumentacja biblioteki `Data.Time`: https://hackage.haskell.org/package/time/docs/Data-Time.html
- Dokumentacja biblioteki `Data.Time.Format`: https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time-Format.html
- Dokumentacja biblioteki `Data.Time.Parse`: https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time-Parse.html