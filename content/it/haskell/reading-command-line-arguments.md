---
title:                "Haskell: Leggere gli argomenti della linea di comando."
programming_language: "Haskell"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/haskell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Perché leggere gli argomenti della riga di comando in Haskell

La lettura degli argomenti dalla riga di comando è un'operazione comune quando si sviluppa un programma Haskell. Gli argomenti della riga di comando possono essere utilizzati per passare informazioni importanti al programma durante l'esecuzione.

## Come fare

Per leggere gli argomenti della riga di comando in Haskell, è necessario utilizzare la funzione `getArgs` del modulo `System.Environment`. Questa funzione restituisce una lista di stringhe contenente gli argomenti passati al programma. Ad esempio:

```Haskell
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    putStrLn $ "Gli argomenti passati sono: " ++ show args
```

Se il programma viene compilato e chiamato da linea di comando con argomenti, ad esempio `./programma arg1 arg2`, l'output sarà:

```
Gli argomenti passati sono: ["arg1", "arg2"]
```

## Approfondimento

Esistono alcune considerazioni importanti da tenere a mente quando si leggono gli argomenti della riga di comando in Haskell. Una di queste è che gli argomenti della riga di comando vengono sempre letti come stringhe, quindi se si vuole utilizzare gli argomenti come numeri o altri tipi di dati è necessario convertirli esplicitamente.

Inoltre, è importante notare che gli argomenti della riga di comando vengono letti nell'ordine in cui sono passati al programma. Ciò significa che se gli argomenti hanno un significato specifico per il programma, è necessario passarli nello stesso ordine ogni volta.

## Vedi anche

- Documentazione ufficiale di `getArgs`: https://hackage.haskell.org/package/base/docs/System-Environment.html#v:getArgs
- Un tutorial su come leggere gli argomenti della riga di comando in Haskell: https://www.haskellforall.com/2013/06/getting-started-with-io.html#command-line-arguments
- Altri modi per gestire gli argomenti della riga di comando in Haskell: https://wiki.haskell.org/Argument_handling