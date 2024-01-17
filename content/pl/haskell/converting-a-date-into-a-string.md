---
title:                "Konwersja daty na ciąg znaków"
html_title:           "Haskell: Konwersja daty na ciąg znaków"
simple_title:         "Konwersja daty na ciąg znaków"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# Jak zamienić datę na ciąg znaków w języku Haskell?

## Co to jest i dlaczego to robimy?
Zamiana daty na ciąg znaków jest częstym zadaniem programistów. Polega ona na konwersji wartości reprezentującej datę (np. 10 marca 2021) na ciąg znaków (np. "10.03.2021"). Jest to przydatne, ponieważ wiele systemów wymaga formatu daty w postaci ciągu znaków.

## Jak to zrobić?
W języku Haskell istnieje kilka sposobów na konwersję daty na ciąg znaków. Przykładowy kod może wyglądać następująco:

```Haskell
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Time.Calendar (Day, fromGregorian)

dateToString :: Day -> String
dateToString date = formatTime defaultTimeLocale "%d.%m.%Y" date

main = do
  let date = fromGregorian 2021 03 10
  putStrLn (dateToString date)
```

W powyższym kodzie wykorzystujemy bibliotekę `Data.Time.Format`, która umożliwia formatowanie daty w podany przez nas sposób. Funkcja `formatTime` przyjmuje dwa argumenty: obiekt reprezentujący format, w którym chcemy otrzymać datę i samą datę. W naszym przykładzie wykorzystujemy domyślny format dla lokalizacji czasu (czyli w tym przypadku polski format daty), ale możemy również podać własny format. Następnie wykorzystujemy funkcję `fromGregorian` z biblioteki `Data.Time.Calendar`, która konwertuje rok, miesiąc i dzień na wartość typu `Day` reprezentującą datę. W naszym przykładzie ustawiamy datę na 10 marca 2021 i wyświetlamy ją na ekranie, wywołując funkcję `dateToString`.

Po uruchomieniu programu, powinniśmy zobaczyć na ekranie: "10.03.2021", czyli naszą datę w postaci ciągu znaków.

## Rzut okiem w głąb
Konwersja daty na ciąg znaków jest popularnym zadaniem w wielu językach programowania, nie tylko w Haskellu. W innych językach możemy użyć innych bibliotek lub funkcji do osiągnięcia tego samego efektu. W Haskellu możemy również wykorzystać inne funkcje związane z obsługą dat, np. `parseTime` czy `getTime`.

Przyjrzyjmy się teraz trochę bliżej użytej przez nas funkcji `formatTime`. Jej pierwszym argumentem jest obiekt typu `TimeLocale`, który definiuje określone formaty dla różnych lokalizacji czasowych. Dzięki temu nasz program będzie zgodny z ustawieniami systemowymi użytkownika, co jest bardzo przydatne w przypadku programów wielojęzycznych.

W naszym przykładzie ustawiliśmy domyślny format dla lokalizacji polskiej, ale możemy również zdefiniować własny. Więcej informacji na temat dostępnych formatów możemy znaleźć w dokumentacji biblioteki.

## Zobacz także
 - [Dokumentacja biblioteki Data.Time.Format](https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time-Format.html)
 - [Inne metody konwersji daty na ciąg znaków w języku Haskell](https://stackoverflow.com/questions/16301512/convert-date-to-string-in-haskell)