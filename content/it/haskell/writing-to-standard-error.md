---
title:                "Scrivere su standard error"
html_title:           "Haskell: Scrivere su standard error"
simple_title:         "Scrivere su standard error"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/haskell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Perché

Scrivere su standard error è un'operazione comune quando si sta scrivendo codice in Haskell. Questo articolo spiega perché è importante e come farlo correttamente.

## Come fare

Utilizzare la funzione `hPutStrLn` per scrivere su standard error. Ecco un esempio di codice:

```Haskell
import System.IO

hPutStrLn stderr "Questo messaggio verrà stampato su standard error"
```

Questo codice importa il modulo `System.IO` che fornisce le funzioni per interfacciarsi con i file di input/output. Poi utilizza `hPutStrLn` per scrivere il messaggio specificato su standard error.

Se si desidera utilizzare una stringa di input fornita dall'utente, è possibile farlo in questo modo:

```Haskell
import System.IO
import System.Console.GetOpt
import System.Environment (getArgs)

main = do
  args <- getArgs
  case args of
    [str] -> hPutStrLn stderr $ "Input dell'utente: " ++ str
    _ -> usage

usage = putStrLn "Utilizzo: mio_programma <input>"

```

In questo codice, la funzione `getArgs` viene utilizzata per ottenere gli argomenti passati al programma da linea di comando. Poi viene utilizzata `case` per gestire il numero di argomenti e, nel caso ne sia passato solo uno, stampare una stringa che contiene l'input dell'utente su standard error utilizzando `hPutStrLn`.

## Approfondimento

Esistono diversi modi per scrivere su standard error in Haskell. Ad esempio, è possibile utilizzare il pacchetto `monads-tf` che fornisce una sintassi più concisa per lavorare con monadi.

Ecco un esempio di codice che utilizza questo pacchetto:

```Haskell
{-# LANGUAGE OverloadedStrings #-}
import Control.Monad.TF
import System.IO

main :: IO ()
main =
    runTF $ do
        putStrLnT "Questo messaggio verrà stampato su standard error"
        liftIO $ hPutStrLn stderr "Questo messaggio verrà stampato utilizzando la monade TF"
```

In questo codice, viene utilizzata la funzione `runTF` per eseguire la monade `TF` che stampa il primo messaggio utilizzando `putStrLnT` e il secondo utilizzando `liftIO` per eseguire l'azione `hPutStrLn` su standard error.

## Vedi anche

- [Documentazione ufficiale di Haskell](https://www.haskell.org/documentation/)
- [Pacchetto monads-tf su Hackage](https://hackage.haskell.org/package/monads-tf)
- [Tutorial su come lavorare con monadi in Haskell](https://mmhaskell.com/monads)