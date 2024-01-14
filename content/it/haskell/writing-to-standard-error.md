---
title:                "Haskell: Scrivere su standard error"
programming_language: "Haskell"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/haskell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Perché

Scrivere su Standard Error è un modo efficiente per gestire gli errori e le eccezioni all'interno di un programma Haskell. Invece di interrompere l'esecuzione del programma, scrivere su Standard Error consente di gestire l'errore e continuare con l'esecuzione del codice.

## Come fare

Per scrivere su Standard Error in Haskell, è necessario importare il modulo `System.IO` e utilizzare la funzione `hPutStrLn` per scrivere una stringa su Standard Error.

```Haskell
import System.IO

main = do
  hPutStrLn stderr "Questo è un messaggio di errore."
```

Output:
```
Questo è un messaggio di errore.
```

## Approfondimento

Scrivere su Standard Error consente di gestire gli errori in modo specifico, poiché questo canale viene utilizzato solo per la stampa degli errori. Inoltre, è possibile eseguire il redirect dei messaggi di errore su un file diverso, invece di visualizzarli sulla console.

Per esempio, nel caso di un'operazione di divisione per zero:

```Haskell
import System.IO

main = do
  putStrLn "Inserisci il primo numero: "
  num1 <- getLine
  putStrLn "Inserisci il secondo numero: "
  num2 <- getLine
  let result = read num1 / read num2 :: Double
  putStrLn ("Il risultato è: " ++ show result)
```

Output:
```
Inserisci il primo numero:
10
Inserisci il secondo numero:
0
*** Exception: divide by zero
```

Nell'output, il messaggio di errore viene stampato sulla console e l'esecuzione del programma viene interrotta. Tuttavia, se aggiungiamo la riga `hPutStrLn stderr "Errore: Divisione per zero."` dentro alla funzione `main` prima di calcolare il risultato, il messaggio di errore verrà stampato su Standard Error invece che sulla console:

```Haskell
import System.IO

main = do
  putStrLn "Inserisci il primo numero: "
  num1 <- getLine
  putStrLn "Inserisci il secondo numero: "
  num2 <- getLine
  hPutStrLn stderr "Errore: Divisione per zero."
  let result = read num1 / read num2 :: Double
  putStrLn ("Il risultato è: " ++ show result)
```

Output:
```
Inserisci il primo numero:
10
Inserisci il secondo numero:
0
Errore: Divisione per zero.
Il risultato è: Infinity
```

Questo è utile quando si vuole gestire l'errore in modo specifico, ad esempio con un handler di eccezioni.

## Vedi anche

- [Modulo System.IO](https://hackage.haskell.org/package/base-4.15.0.0/docs/System-IO.html)
- [Funzione hPutStrLn](https://hackage.haskell.org/package/base-4.15.0.0/docs/System-IO.html#v:hPutStrLn)