---
title:    "Haskell: Scrivere test"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/haskell/writing-tests.md"
---

{{< edit_this_page >}}

## Perché

Scrivere test di programmazione è un'importante pratica per garantire la qualità e l'affidabilità del proprio codice. In questo articolo, esploreremo come scrivere test in Haskell e come possono aiutare a migliorare il processo di sviluppo.

## Come fare

Per scrivere test in Haskell, è possibile utilizzare il framework di test integrato chiamato "HUnit". Questo framework fornisce una sintassi semplice e intuitiva per scrivere e eseguire test.

Ecco un esempio di come scrivere un test di base utilizzando HUnit:

```Haskell
import Test.HUnit

-- Definiamo una funzione che dovrà essere testata
somma :: Int -> Int -> Int
somma x y = x + y

-- Definiamo un test che verifica se la funzione "somma" ritorna il risultato corretto
testSomma :: Test
testSomma = TestCase (assertEqual "La somma di 1 e 2 dovrebbe essere 3" 3 (somma 1 2))

-- Eseguiamo tutti i test definiti sopra
main :: IO ()
main = runTestTT $ TestList [testSomma]
```

Il risultato dell'esecuzione dovrebbe essere qualcosa di simile a questo:

```
### Dati i seguenti test:
 testSomma

### Si riceverà il seguente output:
### Test su "testSomma":
### Passato: True

### Numero di test eseguiti: 1 di 1
### Errore totale del compilatore: 0 di 0

```
In questo esempio, abbiamo definito una semplice funzione "somma" e un test che verifica se il suo output è corretto. Utilizzando la funzione "assertEqual" del framework HUnit, possiamo confrontare il valore di ritorno della funzione con il risultato che ci aspettiamo.

## Approfondimento

Scrivere test in Haskell è importante non solo per verificare la correttezza del codice, ma anche per aiutare a migliorare la progettazione stessa. Infatti, scrivendo test prima di scrivere il codice reale, è possibile avere una maggiore chiarezza sulle aspettative e sui risultati attesi della funzione.

Inoltre, i test sono un ottimo strumento per verificare il comportamento dei refactoring del codice. Se un refactoring causa problemi nei test, è possibile individuare rapidamente la causa e correggerla.

Inoltre, i test possono fornire una documentazione efficace per il codice. Se un'altra persona deve lavorare sul tuo codice, i test possono aiutarla a capire più facilmente cosa fa ogni funzione e come deve essere utilizzata.

Infine, scrivere test è un'ottima abitudine da sviluppare. Non solo migliorerà la qualità del codice, ma renderà anche il processo di debugging più efficiente poiché è possibile isolare i problemi più facilmente.

## Vedi anche

[Sito ufficiale di HUnit](https://hackage.haskell.org/package/HUnit)

[Guida utente di HUnit](https://unit.it/user-guide.html)

[Tutorial su come scrivere test in Haskell](https://abhinavsarkar.net/posts/writing-haskell-tests/)