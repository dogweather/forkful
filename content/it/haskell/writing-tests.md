---
title:                "Scrivere test"
html_title:           "Haskell: Scrivere test"
simple_title:         "Scrivere test"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/haskell/writing-tests.md"
---

{{< edit_this_page >}}

## Perché

Scrivere test è uno degli aspetti più importanti della programmazione. Ciò consente di verificare che il codice funzioni correttamente e di evitare potenziali errori e bug durante il processo di sviluppo. Inoltre, i test possono aiutare a mantenere il codice organizzato e leggibile.

## Come fare

Per scrivere test in Haskell, è necessario utilizzare un framework di testing come HUnit o QuickCheck. Ad esempio, se vogliamo testare una funzione che calcola la somma di due numeri interi, possiamo utilizzare HUnit in questo modo:

```Haskell
-- Importa il modulo HUnit
import Test.HUnit

-- Definizione della funzione da testare
somma :: Int -> Int -> Int
somma x y = x + y

-- Definizione del caso di test
--(nome del test) = TestCase (assertion)
testSomma = TestCase (assertEqual "somma" 5 (somma 2 3))

-- Esecuzione del test
-- restituirà una lista di risultati
main = runTestTT testSomma
```

L'output del test sarà il seguente:

```
Cases: 1  Tried: 1  Errors: 0  Failures: 0
                              
   Cases: 1  Tried: 1  Errors: 0  Failures: 0
```

Il primo riepilogo riporta il numero totale di casi testati, mentre il secondo riassume il singolo caso di test specificato.

Esistono anche altre opzioni per testare il codice in Haskell, come QuickCheck che esegue test di proprietà casuali e rappresenta un approccio più efficiente per testare il codice.

## Approfondimento

Scrivere test in Haskell può sembrare una procedura noiosa e ridondante a volte, specialmente per le funzioni più semplici. Tuttavia, i test possono essere estremamente preziosi quando si tratta di modificare il codice o aggiungere nuove funzionalità, poiché possono rivelare eventuali errori o cambiamenti inaspettati in modo rapido ed efficiente.

Inoltre, i test possono essere di grande aiuto quando si lavora con codebase di grandi dimensioni e complesse, poiché aiutano a mantenere il codice organizzato e facilmente manutenibile.

## Vedi anche

- [HUnit](https://hackage.haskell.org/package/HUnit)
- [QuickCheck](https://hackage.haskell.org/package/QuickCheck)
- [Introduzione al testing in Haskell](http://learnyouahaskell.com/modules#testing)