---
title:                "Haskell: Obliczanie daty w przyszłości lub przeszłości"
simple_title:         "Obliczanie daty w przyszłości lub przeszłości"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Dlaczego
Czasami, jako programiści, musimy ustalić datę w przyszłości lub w przeszłości dla naszych projektów. Może to wynikać z potrzeby przypisania zadania lub wydarzenia do konkretnego dnia lub po prostu z ciekawości. W tym artykule dowiesz się, jak w łatwy sposób obliczyć datę w przyszłości lub przeszłości za pomocą języka programowania Haskell.

# Jak to zrobić
Aby obliczyć datę w przyszłości lub przeszłości w Haskell, musimy użyć pakietu Data.Time, który zapewnia nam wiele funkcji związanych z datami i godzinami.

Najpierw musimy zaimportować ten pakiet do naszego kodu, używając słowa kluczowego `import`.

```Haskell
import Data.Time
```

Następnie, aby ustawić datę w przyszłości lub przeszłości, potrzebujemy dwóch elementów: daty bazowej i ilości dni, które chcemy dodać lub odjąć.

Możemy użyć funkcji `addDays` lub `subtractDays` w zależności od tego, czy chcemy dodać czy odjąć dni od daty bazowej.

Poniżej przedstawiam przykładowy kod obliczający datę 7 dni w przód od daty bieżącej:

```Haskell
main = do
  currentDate <- getCurrentTime
  let newDate = addDays 7 $ utctDay currentDate
  putStrLn $ "Data w przyszłości: " ++ show newDate
```

W tym kodzie używamy funkcji `getCurrentTime` do pobrania bieżącej daty i przypisujemy ją do zmiennej `currentDate`. Następnie, używając `addDays 7` dodajemy 7 dni do daty bieżącej i przypisujemy do zmiennej `newDate`. Na końcu wykorzystujemy funkcję `putStrLn` do wyświetlenia wyniku na ekranie.

Wyjście powyższego kodu będzie wyglądało następująco (przy założeniu, że dzisiaj jest 18 marca 2021 r.):

`Data w przyszłości: 2021-03-25`

Możemy także użyć funkcji `subtractDays` w podobny sposób, aby obliczyć datę w przeszłości. Poniższy przykład pokazuje, jak obliczyć datę 7 dni temu od daty bieżącej:

```Haskell
main = do
  currentDate <- getCurrentTime
  let newDate = subtractDays 7 $ utctDay currentDate
  putStrLn $ "Data w przeszłości: " ++ show newDate
```

Wyjście powyższego kodu (przy założeniu, że dzisiaj jest 18 marca 2021 r.) będzie takie:

`Data w przeszłości: 2021-03-11`

# Deep Dive
W powyższych przykładach używaliśmy funkcji `addDays` i `subtractDays` do obliczania dat w przyszłości lub przeszłości, ale biblioteka Data.Time zapewnia także inne funkcje, takie jak `addWeeks`, `addMonths`, `addYears` i wiele innych, które pozwalają dodać lub odjąć określoną ilość tygodni, miesięcy lub lat od daty bazowej. 

Warto również pamiętać, że daty w Haskell są wyrażone za pomocą typów danych `Day` lub `LocalTime`, co pozwala nam na precyzyjne operacje na datach.

# Zobacz także
- Dokumentacja pakietu Data.Time: https://hackage.haskell.org/package/time/docs/Data-Time.html
- Poradnik dla początkujących w Haskell: https://wiki.haskell.org/Learn_Haskell
- Kursy online z Haskell: https://www.udemy.com/topic/haskell/