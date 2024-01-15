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

## Dlaczego

Masz datę i chcesz ją przekonwertować na string w Haskellu? Jest to częsty problem napotykany przez programistów, którzy pracują z danymi czasowymi. W tym artykule dowiesz się, jak w łatwy sposób przekonwertować datę na string w Haskellu.

## Jak to zrobić

Kodowanie daty w Haskellu nie jest trudne, ale wymaga użycia kilku funkcji. Najpierw musimy zaimportować moduł Data.Time, który jest niezbędny do manipulowania datami.

```Haskell
import Data.Time
```

Następnie musimy stworzyć wartość daty z użyciem funkcji `fromGregorian`, która przyjmuje trzy argumenty: rok, miesiąc i dzień.

```Haskell
let mojaData = fromGregorian 2021 11 25
```

Teraz użyjemy funkcji `formatTime`, aby przekonwertować datę na string. Pierwszy argument tej funkcji to format daty, który chcemy uzyskać. W tym przykładzie użyjemy formatu "dd/mm/yyyy". Drugim argumentem jest data, którą chcemy przekonwertować.

```Haskell
let stringData = formatTime defaultTimeLocale "%d/%m/%Y" mojaData
```

W tym przykładzie ustawiliśmy wartość parametru `defaultTimeLocale` na `en_US`, ale można go zmienić na język polski za pomocą funkcji `localePL`. Teraz, jeśli wyświetlimy wartość `stringData`, powinniśmy zobaczyć datę w formacie "25/11/2021".

```
> stringData
"25/11/2021"
```

## Wnikliwe studium

Musimy uważnie wybrać format daty, ponieważ będzie on wpływał na sposób przekonwertowania daty na string. Przykładowe symbole, które można użyć do budowania formatu, to: `%d` - dzień, `%m` - miesiąc, `%Y` - rok, `%H` - godzina (24-godzinna), `%M` - minuta, `%S` - sekunda. 

Możemy również użyć funkcji `parseTimeM`, aby przekonwertować string na datę lub funkcji `getCurrentTime`, aby uzyskać bieżącą datę. Są to tylko podstawowe elementy obsługi dat w Haskellu, więc warto przejrzeć dokumentację w celu głębszego zrozumienia.

## Zobacz także

- [Data.Time - dokumentacja Haskell](https://hackage.haskell.org/package/time/docs/Data-Time.html)
- [Rozszerzenie Data.Time w Haskellu](https://wiki.haskell.org/Extended_backus%E2%80%93Naur_form#Literate_Haskell)
- [Przekonwertowanie daty na string w języku Haskell](https://www.javatpoint.com/haskell-date-string-conversion)