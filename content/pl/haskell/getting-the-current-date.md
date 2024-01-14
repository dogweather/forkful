---
title:    "Haskell: Uzyskiwanie aktualnej daty"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Dlaczego
W dzisiejszych czasach programowanie staje się jedną z najważniejszych umiejętności, a jednym z najpopularniejszych języków programowania jest Haskell. Dzięki swojej prostocie i funkcjonalnemu podejściu, Haskell stał się wyborem wielu programistów. Jednym z podstawowych elementów programowania jest praca z datami, dlatego też w tym artykule dowiesz się, jak w prosty sposób uzyskać bieżącą datę w Haskell.

## Jak To Zrobić
Aby uzyskać bieżącą datę w Haskell, wystarczy wykorzystać funkcję `getCurrentTime` z modułu `Data.Time`. Oto przykładowy kod:

```Haskell
import Data.Time

main = do
  currentTime <- getCurrentTime
  putStrLn $ "Bieżąca data i czas: " ++ show currentTime
```

W powyższym kodzie, najpierw importujemy moduł `Data.Time`, aby móc użyć funkcji `getCurrentTime`. Następnie, korzystając z konstrukcji `do` wewnątrz funkcji `main`, przypisujemy bieżącą datę do zmiennej `currentTime` i wypisujemy ją na ekranie za pomocą funkcji `putStrLn`. Wykonanie tego kodu powinno dać nam poniższy wynik:

```Haskell
Bieżąca data i czas: 2021-03-29 15:00:00 UTC
```

Możemy również wyświetlić tylko datę lub czas korzystając z funkcji `utctDay` i `utctDayTime`:

```Haskell
putStrLn $ "Bieżąca data: " ++ show (utctDay currentTime)
putStrLn $ "Bieżący czas: " ++ show (utctDayTime currentTime)
```

## Deep Dive
Funkcja `getCurrentTime` pobiera bieżącą datę z systemu operacyjnego w formacie `UTCTime`, co oznacza Universal Time Coordinated. Jest to w zasadzie to samo co Greenwich Mean Time, ale bez uwzględniania przestawek czasowych. Dokładne działanie tej funkcji może się różnić w zależności od systemu operacyjnego, dlatego warto zwrócić uwagę na to, czy wynik jest zgodny z oczekiwaniami.

## Zobacz też
- Dokumentacja modułu `Data.Time`: https://hackage.haskell.org/package/time/docs/Data-Time.html
- Inne przydatne funkcje związane z datami w Haskell: https://hackage.haskell.org/package/time/docs/Data-Time-Clock.html