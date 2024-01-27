---
title:                "Pisanie testów"
date:                  2024-01-19
html_title:           "Bash: Pisanie testów"
simple_title:         "Pisanie testów"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/writing-tests.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Testy to sprawdziany kodu pod kątem poprawności działania. Programiści testują, by wyłapać błędy przed wypuszczeniem oprogramowania, zapewniając jego jakość i stabilność.

## Jak to zrobić:
Haskell wykorzystuje HUnit do pisania testów jednostkowych. Instalujesz przez `cabal install HUnit`. Sprawdź poniżej:

```Haskell
import Test.HUnit

test1 :: Test
test1 = TestCase (assertEqual "for (foo 3)," (1,2) (foo 3))

foo :: Int -> (Int, Int)
foo x = (x - 2, x - 1)

tests :: Test
tests = TestList [TestLabel "test1" test1]

main :: IO Counts
main = runTestTT tests
```

Uruchamiasz testy komendą `runhaskell testfile.hs`. Powinno pokazać coś takiego:

```
Cases: 1  Tried: 1  Errors: 0  Failures: 0
Counts {cases = 1, tried = 1, errors = 0, failures = 0}
```

## Głębsze spojrzenie:
Historia testów w Haskellu to historia HUnit i QuickCheck. HUnit, klon xUnit, pochodzi z około 2000 roku. QuickCheck, który testuje własności, to innowacja stworzona przez Haskella. Alternatywy to Hspec, dla BDD, i doctest, dla testów w dokumentacji. Implementacja polega na tworzeniu funkcji typu `test :: Test`, które potem uruchamiasz przy pomocy `runTestTT`.

## Zobacz również:
- [HUnit User’s Guide](https://hackage.haskell.org/package/HUnit-1.6.2.0/docs/Test-HUnit.html)
- [QuickCheck na GitHub](https://github.com/nick8325/quickcheck)
- [Tutorial Hspec](https://hspec.github.io/)
- [Doctest Introduction](https://github.com/sol/doctest#readme)
