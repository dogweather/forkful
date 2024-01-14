---
title:    "Haskell: Stampa dell'output di debug"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/haskell/printing-debug-output.md"
---

{{< edit_this_page >}}

## Perché

Stampare output di debug è una pratica utile quando si sta sviluppando un programma in Haskell. Può aiutare ad individuare errori e comprendere l'esecuzione del codice.

## Come fare

Per stampare output di debug in Haskell, è sufficiente utilizzare la funzione `print`. Ad esempio:

```
Haskell
    value = 5
    print value
```

Questo codice stamperà il valore 5 sulla console durante l'esecuzione del programma.

## Approfondimento

La funzione `print` in Haskell fa parte del modulo `Prelude` e ha il tipo `Show a => a -> IO ()`. Ciò significa che può essere applicata a qualsiasi tipo che implementi la classe di tipo `Show`, come Int, String o Bool. Inoltre, la funzione `print` restituisce un'azione di input/output (IO), quindi può essere utilizzata all'interno di una monade IO.

Una pratica comune è quella di utilizzare la funzione `trace` del modulo `Debug.Trace` per stampare output di debug condizionali. Ad esempio:

```
Haskell
    import Debug.Trace
    factorial n = trace "funzione di calcolo fattoriale" $ product [1..n]
    main = do
        print "Inserisci un numero:"
        input <- getLine
        let num = read input :: Int
        let result = factorial num
        print result
```

In questo caso, la stringa "funzione di calcolo fattoriale" verrà stampata solo quando la funzione `factorial` viene chiamata.

## Vedi anche

- Documentazione ufficiale di Haskell: https://www.haskell.org/documentation/
- Tutorial di Haskell: https://www.haskell.org/docs/
- Modulo `Debug.Trace` del pacchetto standard di Haskell: https://hackage.haskell.org/package/base/docs/Debug-Trace.html