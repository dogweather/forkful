---
title:                "Haskell: Stampa della risoluzione dei problemi di output"
programming_language: "Haskell"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/haskell/printing-debug-output.md"
---

{{< edit_this_page >}}

## Perché

Stampare output di debug è uno strumento utile per comprendere e risolvere i problemi nel codice Haskell. Può aiutare a identificare errori, tracciare il flusso dei dati e verificare l'esecuzione dei programmi.

## Come Fare

Per stampare output di debug in Haskell si utilizza la funzione `Debug.Trace.trace`. Qui di seguito è riportato un esempio di codice che mostra come utilizzare questa funzione:

```Haskell
import Debug.Trace

factorial :: Int -> Int
factorial n = if n < 0 then error "Il fattoriale è definito solo per numeri positivi!" else
    trace ("Calcolo il fattoriale di " ++ show n) $
    if n == 0 then 1 else n * factorial (n-1)

main = do
  let result = factorial 5
  print result
```

L'output di questo codice sarà:

```
Calcolo il fattoriale di 5
Calcolo il fattoriale di 4
Calcolo il fattoriale di 3
Calcolo il fattoriale di 2
Calcolo il fattoriale di 1
120
```

In questo esempio, la funzione `trace` viene utilizzata per stampare delle informazioni aggiuntive durante l'esecuzione del codice. È possibile utilizzare qualsiasi tipo di dato all'interno della stringa passata alla funzione `trace` utilizzando la funzione `show`.

## Approfondimento

Esistono altri modi per effettuare il debugging in Haskell, come ad esempio l'utilizzo del modulo `Debug.Trace`. Questo modulo contiene diverse funzioni utili, come ad esempio `traceShow`, che stampa sia la stringa passata come primo argomento che il valore passato come secondo argomento.

È importante tenere presente che utilizzare la funzione `trace` è consigliato solo per scopi di debugging. Infatti, se il codice viene eseguito senza la funzione `trace`, il risultato sarà diverso a causa dell'introduzione di chiamate aggiuntive alla funzione `trace`. Quindi, è importante rimuovere tutti i riferimenti alla funzione `trace` prima di eseguire il codice in produzione.

## Vedi Anche

- [Haskell Debugging - Official Documentation](https://wiki.haskell.org/Debugging)
- [Debugging Techniques in Haskell - Real World Haskell](http://book.realworldhaskell.org/read/testing-and-quality-assurance.html#debugging-techniques)