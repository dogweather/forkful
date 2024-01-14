---
title:                "Haskell: Stampa di output di debug"
simple_title:         "Stampa di output di debug"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/haskell/printing-debug-output.md"
---

{{< edit_this_page >}}

## Perché

Debugging è un processo essenziale quando si scrive del codice, e la stampa di output di debug è uno strumento utile per comprendere il flusso di esecuzione del tuo programma. Ciò può aiutare ad identificare e risolvere i problemi nel codice più rapidamente.

## Come fare

Per stampare l'output di debug in Haskell, puoi utilizzare la funzione `putStrLn`, che prende come argomento una stringa e la stampa a schermo. Esempio:

```Haskell
main = do
  putStrLn "Inizio programma"
  putStrLn "Esecuzione codice"
  putStrLn "Fine programma"
```
Questo esempio stampa tre righe di output sullo schermo quando il programma viene eseguito.

## Approfondimento

Quando si utilizza la stampa di debug output, è importante prestare attenzione alla formattazione e all'organizzazione delle informazioni stampate. Puoi utilizzare la funzione `show` per visualizzare il valore di una variabile o espressione in una stringa. Esempio:

```Haskell
main = do
  let x = 5
  putStrLn $ "Il valore di x è " ++ (show x)
```
In questo esempio, la variabile `x` viene convertita in una stringa utilizzando `show` e viene concatenata alla stringa "Il valore di x è". L'output sarà "Il valore di x è 5".

Oltre alla semplice stampa di valori, puoi anche utilizzare funzioni più avanzate come `trace` della libreria `Debug.Trace`, che permette di stampare output di debug senza dover interrompere l'esecuzione del programma.

## Vedi anche

- [Haskell Debugging Guide](https://wiki.haskell.org/Debugging)
- [Debugging in Haskell](https://medium.com/@MarynaLuchko/debugging-in-haskell-3ded9aae871a)
- [Debugging Techniques in Haskell](https://www.reddit.com/r/haskell/comments/6yqs4e/debugging_techniques_in_haskell/)