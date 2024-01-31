---
title:                "Scrivere test"
date:                  2024-01-19
html_title:           "Arduino: Scrivere test"
simple_title:         "Scrivere test"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/haskell/writing-tests.md"
---

{{< edit_this_page >}}

## Che cosa e perché?
Scrivere test nel codice serve a verificare che il software funzioni come atteso. I programmatori lo fanno per prevenire bug e garantire che le modifiche future non rompano funzionalità esistenti.

## Come fare:
Haskell utilizza HUnit e QuickCheck come librerie popolari per il testing. Ecco un esempio semplice con HUnit:

```Haskell
import Test.HUnit

testAddition = TestCase (assertEqual "Verifica che 1 + 1 uguale 2" 2 (1 + 1))

main :: IO ()
main = runTestTT testAddition >>= print
```

Output previsto:

```
Cases: 1  Tried: 1  Errors: 0  Failures: 0
Counts {cases = 1, tried = 1, errors = 0, failures = 0}
```

Con QuickCheck per test basati su proprietà:

```Haskell
import Test.QuickCheck

prop_reverseTwice :: [Int] -> Bool
prop_reverseTwice list = reverse (reverse list) == list

main :: IO ()
main = quickCheck prop_reverseTwice
```
Output previsto:

```
+++ OK, passed 100 tests.
```

## Analisi dettagliata:
HUnit è ispirato a JUnit e permette di scrivere test di unità. QuickCheck implementa test basati su proprietà con input generati casualmente. Storicamente, QuickCheck ha influenzato il testing in altri linguaggi con concetti simili di generazione di test cases. Per implementare i test in Haskell, è importante comprendere il Controllo dei Tipi e le Funzioni Pure per un'efficacia ottimale.

## Guarda anche:
- HUnit: [http://hackage.haskell.org/package/HUnit](http://hackage.haskell.org/package/HUnit)
- QuickCheck: [http://hackage.haskell.org/package/QuickCheck](http://hackage.haskell.org/package/QuickCheck)
- Un articolo introduttivo ai test in Haskell: [https://wiki.haskell.org/Introduction_to_HUnit](https://wiki.haskell.org/Introduction_to_HUnit)
- Una guida per approfondire QuickCheck: [https://begriffs.com/posts/2017-01-14-design-use-quickcheck.html](https://begriffs.com/posts/2017-01-14-design-use-quickcheck.html)
