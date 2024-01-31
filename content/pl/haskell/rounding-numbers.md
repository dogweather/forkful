---
title:                "Zaokrąglanie liczb"
date:                  2024-01-26T03:45:03.924780-07:00
model:                 gpt-4-0125-preview
simple_title:         "Zaokrąglanie liczb"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/rounding-numbers.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Zaokrąglanie liczb oznacza dostosowanie ich do najbliższej liczby całkowitej lub określonego miejsca po przecinku. Programiści zaokrąglają liczby, aby kontrolować precyzję, dostosowywać wyniki dla prezentacji użytkownika lub redukować koszty obliczeń dla operacji zmiennoprzecinkowych.

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
