---
title:                "Konwersja daty na ciąg znaków"
html_title:           "Clojure: Konwersja daty na ciąg znaków"
simple_title:         "Konwersja daty na ciąg znaków"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?
Przekształcanie daty na ciąg, zwane też serializacją dat, polega na zapisie obiektu daty jako łatwo czytelnego tekstu. Programiści robią to, aby przechować i przekazać datę w sposób, który będzie zrozumiały dla człowieka lub różnych usług i aplikacji.

## Jak to zrobić:

Haskell oferuje funkcję `formatTime` z pakietu `Data.Time.Format` do konwersji daty na ciąg. Zobaczmy, jak to działa:
```Haskell
import Data.Time
import Data.Time.Format

main = do
    czas <- getCurrentTime
    putStrLn $ formatTime defaultTimeLocale "%d-%m-%Y" czas
```
Gdy uruchomisz powyższy program, wydrukuje dzisiejszą datę w formacie "DD-MM-YYYY". Na przykład, 01-01-2022.

## Na głębszy poziom

1. **Kontekst historyczny**: Funkcja `formatTime` negatywnie wpływa na wydajność, gdy jest używana dużo razy, ponieważ wymaga za każdym razem interpretacji ciągu formatującego. Problem ten został rozwiązany poprzez wprowadzenie funkcji `formatTimeM` w GHC 7.10, która ma lepszą wydajność.

2. **Alternatywy**: Istnieje wiele bibliotek Haskell, które oferują dodatkowe funkcje formatowania daty, takie jak `time-format` i `date-cache`. 

3. **Szczegóły implementacyjne**: `formatTime` korzysta z `defaultTimeLocale`, który jest odzwierciedleniem lokalnego ustawienia daty i czasu na twoim komputerze. Możesz dostosować format daty, dostarczając własny ciąg formatujący.

## Zobacz także

1. Dokumentacja pakietu `Data.Time.Format`: http://hackage.haskell.org/package/time-1.10/docs/Data-Time-Format.html
2. Dokumentacja pakietu `defaultTimeLocale`: http://hackage.haskell.org/package/time-1.10/docs/Data-Time-Format-Locale.html
3. Wątek na StackOverflow na temat formatowania czasu w Haskellu: https://stackoverflow.com/questions/19280527/how-to-nicely-format-time
4. Blog na temat formatowania dat w Haskellu: https://rosettacode.org/wiki/Date_format#Haskell