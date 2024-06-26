---
date: 2024-01-26 03:45:03.924780-07:00
description: "Jak to zrobi\u0107: Haskell u\u017Cywa funkcji `round`, `ceiling`, `floor`\
  \ oraz `truncate` z `Prelude` do operacji zaokr\u0105glania."
lastmod: '2024-03-13T22:44:35.446373-06:00'
model: gpt-4-0125-preview
summary: "Haskell u\u017Cywa funkcji `round`, `ceiling`, `floor` oraz `truncate` z\
  \ `Prelude` do operacji zaokr\u0105glania."
title: "Zaokr\u0105glanie liczb"
weight: 13
---

## Jak to zrobić:
Haskell używa funkcji `round`, `ceiling`, `floor` oraz `truncate` z `Prelude` do operacji zaokrąglania.

```haskell
import Prelude

main :: IO ()
main = do
  let num = 3.567
  print $ round num    -- 4
  print $ ceiling num  -- 4
  print $ floor num    -- 3
  print $ truncate num -- 3
  
  -- Zaokrąglanie do określonego miejsca po przecinku nie znajduje się w Prelude.
  -- Oto funkcja niestandardowa:
  let roundTo n f = (fromInteger $ round $ f * (10^n)) / (10.0^^n)
  print $ roundTo 1 num -- 3.6
```

## Dogłębna analiza
Historycznie zaokrąglanie ma znaczenie w analizie numerycznej i informatyce, ponieważ jest kluczowe do minimalizowania akumulacji błędów w obliczeniach, szczególnie przed standaryzacją reprezentacji zmiennoprzecinkowej przez IEEE 754.

Do czego zaokrąglać? `round` zabiera cię do najbliższej liczby całkowitej—w górę lub w dół. `ceiling` i `floor` zawsze zaokrąglają w górę lub w dół do najbliższej liczby całkowitej, odpowiednio, natomiast `truncate` po prostu odrzuca cyfry po przecinku.

Alternatywy dla tych funkcji mogą obejmować niestandardową logikę, jak nasza `roundTo`, albo możesz korzystać z bibliotek (takich jak Data.Fixed) dla bardziej złożonych wymagań.

Uważaj na nieoczekiwane wyniki spowodowane tym, jak Haskell obsługuje przypadki pośrednie w `round` (zaokrągla do najbliższej parzystej liczby).

## Zobacz również
- Dokumentacja Haskell Prelude dla funkcji zaokrąglania: https://hackage.haskell.org/package/base-4.16.1.0/docs/Prelude.html
- Wiki Haskell na temat arytmetyki zmiennoprzecinkowej: https://wiki.haskell.org/Floating_point_arithmetic
- Standard IEEE 754-2008 dla więcej informacji o tym, jak obsługuje się liczby zmiennoprzecinkowe w wielu językach: https://ieeexplore.ieee.org/document/4610935
