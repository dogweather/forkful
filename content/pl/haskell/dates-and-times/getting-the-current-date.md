---
aliases:
- /pl/haskell/getting-the-current-date/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:09:37.423534-07:00
description: "Pobranie aktualnej daty w Haskellu polega na uzyskaniu bie\u017C\u0105\
  cego czasu systemu i przekszta\u0142ceniu go na czytelny format daty. Programi\u015B\
  ci robi\u0105 to, aby\u2026"
lastmod: 2024-02-18 23:08:49.663440
model: gpt-4-0125-preview
summary: "Pobranie aktualnej daty w Haskellu polega na uzyskaniu bie\u017C\u0105cego\
  \ czasu systemu i przekszta\u0142ceniu go na czytelny format daty. Programi\u015B\
  ci robi\u0105 to, aby\u2026"
title: Pobieranie aktualnej daty
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
