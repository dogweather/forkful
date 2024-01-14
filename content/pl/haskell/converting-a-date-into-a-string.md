---
title:                "Haskell: Konwertowanie daty na ciąg znaków."
programming_language: "Haskell"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

W programowaniu często zdarza się potrzeba konwertowania daty na napis. Może to być potrzebne do wyświetlenia daty w czytelny sposób lub do przetwarzania danych. W tym artykule dowiecie się jak wykonać tę operację w języku Haskell.

## Jak to zrobić

Konwersja daty na napis w Haskell jest bardzo prosta dzięki modułowi Data.Time.Format. Wystarczy podać formatowanie, a następnie przekażemy datę do funkcji formatTime. Poniżej znajdują się przykładowy kod oraz jego wynik dla daty 1 lutego 2021 r.

```Haskell
import Data.Time.Format

main = do
  let date = fromGregorian 2021 02 01 -- tworzymy datę
  let format = "%d.%m.%Y" -- formatowanie, %d oznacza dzień, %m miesiąc, a %Y rok
  let formatedDate = formatTime defaultTimeLocale format date -- wywołujemy funkcję formatTime
  print formatedDate -- wyświetlamy wynik

```

```
01.02.2021
```

Możemy również wskazać strefę czasową lub usunąć znak "-" z daty, gdy używamy zewnętrznych danych w formacie ISO.

## Głębsze spojrzenie

Warto zaznaczyć, że formatTime korzysta z typu TimeLocale, który zawiera informacje o tym, jak należy interpretować symbole w formacie. Domyślnie korzysta z lokalnej strefy czasowej, ale można ją zmienić, korzystając z funkcji newTimeLocale.

Ponadto, jeśli potrzebujemy bardziej skomplikowanego formatu daty, możemy skorzystać z funkcji parseTimeM, która umożliwia przetworzenie napisu o określonym formacie na datę.

## Zobacz również

- [Dokumentacja modułu Data.Time.Format](https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time-Format.html)
- [List formatów dat w Haskell](https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time-Format.html#t:TimeLocale)