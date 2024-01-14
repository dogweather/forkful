---
title:    "Haskell: Obliczanie daty w przyszłości lub przeszłości"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Dlaczego

Kalkulowanie daty w przyszłości lub przeszłości może być bardzo przydatne w wielu projektach, takich jak tworzenie aplikacji do rezerwacji lub planowania wydarzeń. Dzięki temu zabiegowi można łatwo obliczyć datę w dowolnej odległości od dzisiejszego dnia, co może zaoszczędzić wiele czasu i wysiłku w przyszłości.

## Jak to zrobić

Aby wykonać obliczenia daty w przyszłości lub przeszłości w Haskell, potrzebujemy tylko kilku prostych kroków. Najpierw musimy zaimportować moduł Data.Time, który umożliwi nam pracę z datami w funkcjonalnym języku programowania. Następnie użyjemy funkcji "addDays" lub "addGregorianYearsClip" w celu dodania odpowiedniej liczby dni lub lat do bieżącej daty.

```Haskell
import Data.Time

-- Dodawanie 10 dni do dzisiejszej daty
dzisiaj <- getCurrentTime
let dzisiaj_plus_10_dni = addDays 10 dzisiaj
```

## Głębokie zagłębienie

Oprócz dodawania dni lub lat, możemy również wykonać bardziej złożone obliczenia daty, takie jak obliczenie daty w przeszłości. Możemy to osiągnąć poprzez użycie ujemnych wartości w funkcjach dodających, takich jak "addDays" lub "addGregorianYearsClip". Ponadto, w module Data.Time istnieją również inne funkcje, takie jak "diffDays" czy "diffGregorianYearsClip", które pozwalają na obliczenie różnicy między dwoma datami.

```Haskell
-- Obliczanie różnicy między dwoma datami
let data1 = fromGregorian 2020 10 01
let data2 = fromGregorian 2020 09 01
let roznica = diffDays data1 data2
```

## Zobacz również

1. https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time.html
2. https://www.haskell.org/hoogle/?q=addDays
3. https://www.haskell.org/hoogle/?q=diffDays