---
title:                "Przetwarzanie daty ze łańcucha znaków"
date:                  2024-01-20T15:36:32.151862-07:00
simple_title:         "Przetwarzanie daty ze łańcucha znaków"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Co i dlaczego? Parsing to proces przekształcenia tekstu na dane o określonej strukturze, w tym przypadku - daty. Programiści parsują daty, aby móc manipulować i wykorzystywać je w aplikacjach.

## How to:
Jak to zrobić:

```Haskell
import Data.Time.Format
import Data.Time.Clock

-- parsowanie daty
parseDate :: String -> Maybe UTCTime
parseDate = parseTimeM True defaultTimeLocale "%Y-%m-%d" 

main :: IO ()
main = do
  let exampleDate = "2021-09-17"
  print $ parseDate exampleDate
```

Wyjście:
```
Just 2021-09-17 00:00:00 UTC
```

## Deep Dive
Głębsze spojrzenie: Historia funkcji parse działających na datach sięga czasów pierwszych komputerów. W Haskellu, biblioteka `Data.Time.Format` pozwala na elastyczne parsowanie i formatowanie dat. Alternatywami są biblioteki takie jak `time` i `old-time`. Implementacja wykorzystuje 'locale', czyli ustawienia regionalne, które decydują o formacie daty, oraz specyfikatory formatu, takie jak "%Y-%m-%d" dla "rok-miesiąc-dzień".

## See Also
Zobacz także:

- [Haskell Time Library](https://hackage.haskell.org/package/time)
- [LYAHFGG: Dates and Times](http://learnyouahaskell.com/input-and-output#dates-and-times)
