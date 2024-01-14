---
title:                "Haskell: Scrivere test"
simple_title:         "Scrivere test"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/haskell/writing-tests.md"
---

{{< edit_this_page >}}

## Perché scrivere test in Haskell?

Scrivere test può sembrare noioso e dispendioso di tempo, ma in realtà è un'attività molto importante per qualsiasi linguaggio di programmazione, compreso Haskell. I test ci permettono di verificare l'integrità del nostro codice e di individuare eventuali bug, evitando così problemi maggiori una volta che il nostro programma è in produzione. Inoltre, scrivere test ci aiuta a comprendere meglio il nostro codice e a tenere traccia dei cambiamenti che apportiamo.

## Come scrivere test in Haskell

Per scrivere test in Haskell, è necessario utilizzare un modulo chiamato "Test.HUnit", che fornisce funzioni utili per creare e eseguire test. Di seguito è riportato un esempio di codice che utilizza HUnit per testare una semplice funzione addizione:

```Haskell
import Test.HUnit

-- Definizione della funzione "addizione"
addizione :: Int -> Int -> Int
addizione x y = x + y

-- Test della funzione con due input diversi
testAddizione = TestCase (assertEqual "1+2 should be equal to 3" 3 (addizione 1 2))
testAddizione2 = TestCase (assertEqual "-5+10 should be equal to 5" 5 (addizione (-5) 10))

-- Lista dei test da eseguire
tests = TestList [TestLabel "testAddizione" testAddizione, TestLabel "testAddizione2" testAddizione2]

-- Esecuzione dei test
main = do
    runTestTT tests
```

L'output che otterremo dall'esecuzione di questo programma sarà:

```bash
Cases: 2 attempted, 2 successes
Counts {cases = 2, tried = 2, errors = 0, failures = 0}
```

Possiamo vedere che entrambi i test sono stati superati, il che significa che la nostra funzione "addizione" funziona correttamente per questi due input. In caso di errori, HUnit ci indicherà quale test è fallito e quale era il risultato aspettato.

## Approfondimenti su come scrivere test in Haskell

Oltre ad HUnit, esistono altre librerie utili per scrivere test in Haskell, come ad esempio "QuickCheck", che ci permette di generare automaticamente una grande quantità di input differenti per testare la nostra funzione. Inoltre, essere in grado di scrivere test in Haskell significa capire meglio i concetti di immutabilità e funzioni pure, fondamentali nella programmazione funzionale.

## Vedi anche
- [Documentazione ufficiale di HUnit](https://hackage.haskell.org/package/HUnit)
- [QuickCheck su Hackage](https://hackage.haskell.org/package/QuickCheck)
- [Esempio di test con QuickCheck](https://www.fpcomplete.com/blog/2017/07/functional-quickcheck-30-in-minutes/)