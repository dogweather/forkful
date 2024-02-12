---
title:                "Pobieranie aktualnej daty"
aliases:
- /pl/haskell/getting-the-current-date.md
date:                  2024-02-03T19:09:37.423534-07:00
model:                 gpt-4-0125-preview
simple_title:         "Pobieranie aktualnej daty"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/getting-the-current-date.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Co i dlaczego?
Pobranie aktualnej daty w Haskellu polega na uzyskaniu bieżącego czasu systemu i przekształceniu go na czytelny format daty. Programiści robią to, aby wykonywać operacje oparte na dacie, takie jak logowanie, planowanie zadań czy znakowanie czasem zdarzeń w aplikacjach.

## Jak to zrobić:
Standardowa biblioteka Haskella, `base`, dostarcza moduł `Data.Time`, który oferuje funkcjonalność do pracy z datami i czasem. Oto jak użyć go do uzyskania aktualnej daty:

```haskell
import Data.Time (getCurrentTime, utctDay)

main :: IO ()
main = do
    now <- getCurrentTime
    let today = utctDay now
    print today
```

Przykładowe wyjście:
```
2023-04-12
```

Dla większej elastyczności, takiej jak formatowanie daty czy praca z różnymi strefami czasowymi, biblioteka `time` jest nieoceniona. Oto jak można sformatować aktualną datę:

```haskell
import Data.Time

main :: IO ()
main = do
    now <- getCurrentTime
    timezone <- getCurrentTimeZone
    let zoneNow = utcToLocalTime timezone now
    putStrLn $ formatTime defaultTimeLocale "%Y-%m-%d" zoneNow
```

To drukuje aktualną datę w formacie `RRRR-MM-DD`, dostosowaną do lokalnej strefy czasowej.

Dodatkowo, dla wsparcia biblioteki firm trzecich, `time` jest wysoce polecana i często używana w społeczności Haskella ze względu na jej rozległe możliwości manipulacji datą i czasem. Przykłady powyżej wykorzystują tę bibliotekę.

Jeśli potrzebujesz bardziej wszechstronnej manipulacji datą, w tym parsowania ze stringów czy operacji arytmetycznych z datami i czasem, eksploracja dodatkowych funkcji w ramach `Data.Time` będzie korzystna.
