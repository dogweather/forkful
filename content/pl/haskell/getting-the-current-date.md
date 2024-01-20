---
title:                "Pobieranie aktualnej daty"
date:                  2024-01-20T15:14:40.315157-07:00
html_title:           "Bash: Pobieranie aktualnej daty"
simple_title:         "Pobieranie aktualnej daty"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Co & Dlaczego?
Pobieranie aktualnej daty to procedura uzupełniająca aplikacje o informację czasową. Programiści wykorzystują datę do logowania zdarzeń, okresowych funkcji i danych czasozależnych. 

## Jak to zrobić:
W Haskellu możemy użyć pakietu `time` do pracy z datą i czasem:

```haskell
import Data.Time

main :: IO ()
main = do
    currentDate <- getCurrentTime
    print currentDate
```
Wykonanie powyższego kodu wyświetli aktualną datę i czas w formacie UTC, np.:

```
2023-04-05 12:34:56.789876 UTC
```

## Deep Dive
Pobieranie daty w Haskellu wydaje się trywialne, ale warto poznać kilka faktów:
1. Historia: Moduł `Data.Time` pojawił się w Haskellu jako część Package time, który ewoluował na przestrzeni lat, aby zapewnić bardziej obszerne i wszechstronne wsparcie dla obliczeń daty i czasu.
2. Alternatywy: Poza `Data.Time`, istnieją inne biblioteki, np. `old-time`, ale są mniej preferowane z uwagi na ograniczenia i starszą konstrukcję.
3. Szczegóły implementacji: `getCurrentTime` pochodzi z systemu operacyjnego, który odpowiada za śledzenie czasu UTC. Haskell jedynie udostępnia przyjazny interfejs do tych informacji.

## Zobacz również
- [Dokumentacja pakietu `time`](https://hackage.haskell.org/package/time)
- [Haskell.org – Artykuły o Data.Time](https://wiki.haskell.org/Time)
- [SO - Haskell: Jak uzyskać aktualną datę i czas](https://stackoverflow.com/questions/4702325/how-to-get-the-current-date-and-time-in-haskell)