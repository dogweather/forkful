---
date: 2024-01-20 17:36:41.610940-07:00
description: "How to: Jak to zrobi\u0107? W Haskellu, u\u017Cywamy biblioteki `time`\
  \ i jej funkcji `formatTime` do konwersji daty na tekst."
lastmod: '2024-04-05T22:37:44.164547-06:00'
model: gpt-4-1106-preview
summary: "Jak to zrobi\u0107? W Haskellu, u\u017Cywamy biblioteki `time` i jej funkcji\
  \ `formatTime` do konwersji daty na tekst."
title: "Konwersja daty na \u0142a\u0144cuch znak\xF3w"
weight: 28
---

## How to:
Jak to zrobić? W Haskellu, używamy biblioteki `time` i jej funkcji `formatTime` do konwersji daty na tekst:

```Haskell
import Data.Time

-- Zakładamy, że mamy już obiekt `time :: UTCTime`
let dateString = formatTime defaultTimeLocale "%Y-%m-%d" time
-- dateString to teraz "YYYY-MM-DD"
```

Przykładowy wynik:

```
"2023-04-12"
```

## Deep Dive
Głębsze spojrzenie: Biblioteka `time` jest standardem w Haskellu do pracy z czasem. Została wprowadzona, by ułatwić manipulację datami i czasem, udostępniając bogatą funkcjonalność. Alternatywą może być biblioteka `Data.Time.Format`, która oferuje podobne możliwości. Przy konwersji daty na tekst ważne jest określenie poprawnego formatu - `%Y-%m-%d` to międzynarodowy standard ISO 8601 dla dat. 

Implementacja `formatTime` faktycznie używa wyrażeń formatujących, by określić, jak finalny tekst będzie wyglądał. Możemy wybrać różne składniki daty, jak rok, miesiąc, dzień, godzina, czy strefę czasową, by dostosować wynikowy tekst do naszych potrzeb. Na przykład, `%A, %d %B %Y` da nam pełną datę z nazwą dnia tygodnia i miesiąca w tekście.

## See Also
Zobacz też:

- [Haskell time library on Hackage](https://hackage.haskell.org/package/time)
- [Formatting Time and Date in Haskell](https://hackage.haskell.org/package/time-1.9.1/docs/Data-Time-Format.html)
- [Learn You a Haskell for Great Good! – Dates and Times](http://learnyouahaskell.com/zippers#time-machine)
