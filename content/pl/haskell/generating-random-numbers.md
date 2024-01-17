---
title:                "Generowanie losowych liczb"
html_title:           "Haskell: Generowanie losowych liczb"
simple_title:         "Generowanie losowych liczb"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Generowanie losowych liczb to proces wykorzystywany przez programistów do tworzenia przypadkowych danych w celu testowania i symulacji różnych scenariuszy. Jest to przydatne w tworzeniu gier, algorytmów lub w celu analizowania zachowania systemów.

## Jak to zrobić:

Możemy generować losowe liczby w Haskellu za pomocą funkcji `random`. Na przykład, jeśli chcemy wygenerować liczbę całkowitą od 1 do 10, możemy użyć następującego kodu:

```Haskell
import System.Random

main = do
    num <- randomRIO (1,10)
    print num
```

Powyższy kod wykorzystuje moduł `System.Random` i funkcję `randomRIO` do wygenerowania losowej liczby całkowitej w zakresie od 1 do 10. Wynik zostanie wyświetlony w konsoli.

## Vertigo

Generowanie losowych liczb jest procesem wykorzystywanym w wielu językach programowania i istnieje wiele alternatywnych sposobów, jak np. wykorzystanie generatorów liczb pseudolosowych, które są oparte na złożonych obliczeniach matematycznych. W Haskellu, funkcja `random` wykorzystuje generator liczb pseudolosowych o nazwie Mersenne Twister.

## Zobacz także:

Możesz przeczytać więcej o funkcji `random` i innych funkcjach z modułu `System.Random` w oficjalnej dokumentacji Haskell (https://hackage.haskell.org/package/random). Inną przydatną funkcją do generowania losowych danych w Haskellu jest `randomIO`, która jest podobna do `randomRIO`, ale nie wymaga podawania przedziału.