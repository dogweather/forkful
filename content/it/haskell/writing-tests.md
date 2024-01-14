---
title:                "Haskell: Scrivere test"
programming_language: "Haskell"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/haskell/writing-tests.md"
---

{{< edit_this_page >}}

## Perché

Scrivere test di programmazione è un processo importante per garantire che il nostro codice funzioni correttamente e che eventuali modifiche non rompano funzionalità esistenti. I test ci permettono di avere maggiore sicurezza e fiducia nel nostro codice.

## Come

Scrivere test in Haskell è semplice e diretto. Iniziamo creando una nuova cartella per il nostro progetto e creiamo un file `tests.hs` all'interno di essa. Utilizzeremo il framework di test `HUnit`, quindi è necessario importarlo come segue:

```Haskell
import Test.HUnit
```

Possiamo quindi iniziare a scrivere il nostro primo test utilizzando la funzione `assertEqual`, che confronta il valore atteso con il valore ottenuto. Ad esempio, se vogliamo testare la funzione `double` che raddoppia un numero, il nostro test potrebbe essere così:

```Haskell
testDouble = TestCase (assertEqual "La funzione double non funziona correttamente" 8 (double 4))
```

Per eseguire questo test, dobbiamo creare una suite di test utilizzando la funzione `TestList` e passare tutti i test che vogliamo eseguire come argomento. Ad esempio, possiamo creare una suite che contiene solo il nostro testDouble:

```Haskell
tests = TestList [testDouble]
```

Infine, possiamo eseguire effettivamente la suite di test utilizzando la funzione `runTestTT`:

```Haskell
main = runTestTT tests
```

Eseguendo questo programma, vedremo l'output che ci dirà se il nostro test ha avuto successo o meno. Nel nostro caso, il testDouble dovrebbe avere successo poiché la funzione `double` raddoppia correttamente il numero 4.

## Deep Dive

Scrivere test ci permette di avere una migliore comprensione del nostro codice, in quanto dobbiamo pensare al possibile input e output della nostra funzione. Inoltre, ci aiuta a identificare e correggere eventuali bug o problemi nel codice prima che essi si manifestino in produzione.

Un altro aspetto importante dei test è che ci permettono di effettuare modifiche al codice con maggiore sicurezza, in quanto possiamo eseguire i test per verificare che tutti i cambiamenti non abbiano introdotto bug o comportamenti indesiderati.

Inoltre, scrivere test ci aiuta a sviluppare codice più modulare e manutenibile. Infatti, se il nostro codice è ben testato, possiamo facilmente identificare dove e come apportare modifiche senza dover rischiare di rompere funzionalità esistenti.

## Vedi anche

- [HUnit QuickCheck](https://hackage.haskell.org/package/HUnit-QuickCheck) - una estensione per HUnit che permette di scrivere test proprietari usando il framework QuickCheck.
- [Sito ufficiale di Haskell](https://www.haskell.org/) - per maggiori informazioni su Haskell e la programmazione funzionale in generale.
- [Documentazione di HUnit](https://hackage.haskell.org/package/HUnit) - per una maggiore comprensione dei concetti e delle funzioni utilizzate in questo post.