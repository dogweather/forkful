---
title:                "Haskell: Konwertowanie daty na ciąg znaków"
simple_title:         "Konwertowanie daty na ciąg znaków"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Konwersja daty do ciągu znaków jest niezbędnym narzędziem w wielu aplikacjach, szczególnie w tych, które są powiązane z bazami danych. W Haskellu istnieje wiele różnych sposobów na dokonanie tej konwersji, dlatego chcielibyśmy dziś przybliżyć Wam ten temat.

## Jak to zrobić

Poniżej przedstawiamy kilka przykładów kodu w Haskellu, pokazujących różne sposoby konwersji daty do ciągu znaków:

```Haskell
import Data.Time.Format
import Data.Time.Calendar

-- Konwersja daty do formatu "dd/mm/rrrr"
dateToString1 :: Day -> String
dateToString1 = formatTime defaultTimeLocale "%d/%m/%Y"

-- Konwersja daty do formatu "mmm dd, rrrr"
dateToString2 :: Day -> String
dateToString2 = formatTime defaultTimeLocale "%b %d, %Y"

-- Konwersja daty do formatu "rrrr-mm-dd"
dateToString3 :: Day -> String
dateToString3 = formatTime defaultTimeLocale "%Y-%m-%d"
```

Powyższe funkcje wykorzystują moduł `Data.Time.Format` oraz `Data.Time.Calendar` w celu przeprowadzenia konwersji. Każda z tych funkcji przyjmuje jako argument wartość typu `Day` reprezentującą datę i zwraca ciąg znaków w wybranym formacie.

Przykładowe wywołanie funkcji `dateToString1` dla daty 2 lutego 2021 roku zwróci ciąg znaków "02/02/2021", funkcja `dateToString2` zwróci "Feb 02, 2021", a funkcja `dateToString3` zwróci "2021-02-02".

Warto zauważyć, że funkcja `formatTime` wymaga również ustawienia odpowiedniego locale za pomocą `defaultTimeLocale`, co umożliwia wyświetlanie dat w różnych językach.

## Głębsza analiza

Konwersja daty do ciągu znaków w Haskellu jest możliwa dzięki wykorzystaniu wielu wbudowanych funkcji i modułów, takich jak `import Data.Time`, `import Data.Time.Format` oraz `import Data.Time.Calendar`. Istnieje również możliwość definiowania własnych formatów daty za pomocą funkcji `formatTime`. Podczas wyboru odpowiedniego sposobu konwersji należy zwrócić uwagę na ustawienia locale oraz wybrane formaty, tak aby odpowiednio odwzorować potrzeby aplikacji, w której dane są wykorzystywane.

## Zobacz także

- Dokumentacja modułu Data.Time.Format - https://hackage.haskell.org/package/time/docs/Data-Time-Format.html
- Zręczny przewodnik po Data.Time w Haskellu - https://www.fpcomplete.com/blog/2015/06/string-conversion-error-handling-timezones-haskell/
- Przykłady użytkowe modułu Data.Time - https://wiki.haskell.org/wikiupload/5/5e/TMR-Issue13.pdf