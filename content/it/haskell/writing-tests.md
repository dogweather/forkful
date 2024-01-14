---
title:    "Haskell: Scrivere test"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

# Perché

Scrivere test di codice può sembrare una perdita di tempo, ma in realtà è un'attività importante che può migliorare la qualità del tuo codice. I test ti aiutano a prevenire bug, a identificare problemi prima che si verifichino e a fornire una documentazione vivente del tuo codice.

# Come Fare

Per scrivere test efficaci in Haskell, è necessario utilizzare il modulo `Test.HUnit`, che fornisce una libreria per eseguire test unitari. Il seguente codice è un esempio di test di una funzione per calcolare l'area di un triangolo:

```Haskell
import Test.HUnit

-- Funzione per calcolare l'area di un triangolo
areaTriangolo :: Double -> Double -> Double -> Double
areaTriangolo a b c = sqrt (s * (s-a) * (s-b) * (s-c))
    where s = (a + b + c) / 2

-- Test per verificare che la funzione calcoli correttamente l'area
testAreaTriangolo = TestCase (assertEqual "Area del triangolo con lati 3, 4 e 5" 6.0 (areaTriangolo 3 4 5))

-- Elenco dei test da eseguire
tests = TestList [TestLabel "testAreaTriangolo" testAreaTriangolo]

-- Esecuzione dei test
main = do
    runTestTT tests
    return ()
```

L'output dovrebbe essere il seguente:

```
Cases: 1  Tried: 1  Errors: 0  Failures: 0
Cases: 1  Tried: 0  Errors: 0  Failures: 0
```

Questo significa che il test è stato eseguito correttamente e che la funzione `areaTriangolo` ha prodotto il risultato atteso.

# Approfondimento

Quando si scrivono test, è importante considerare tutti i possibili scenari e casi limite. Inoltre, è consigliabile seguire il principio di "una funzione, un test": ogni aspetto della funzione dovrebbe essere testato in modo indipendente.

In Haskell, è possibile utilizzare anche il modulo `HSpec` per scrivere test più descrittivi e organizzati in una struttura gerarchica. Inoltre, è consigliabile scrivere anche test di integrazione per verificare che le varie parti del codice funzionino correttamente insieme.

# Vedi Anche

- [Documentazione del modulo `Test.HUnit`][1]
- [Documentazione del modulo `HSpec`][2]
- [Tutorial su come scrivere test in Haskell][3]

[1]: https://hackage.haskell.org/package/HUnit/docs/Test-HUnit.html
[2]: https://hspec.github.io/
[3]: https://www.tutorialspoint.com/haskell/haskell_unit_testing.htm