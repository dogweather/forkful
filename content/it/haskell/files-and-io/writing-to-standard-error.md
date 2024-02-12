---
title:                "Scrivere sull'errore standard"
aliases:
- /it/haskell/writing-to-standard-error.md
date:                  2024-02-03T19:33:14.636659-07:00
model:                 gpt-4-0125-preview
simple_title:         "Scrivere sull'errore standard"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/haskell/writing-to-standard-error.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cosa & Perché?
Scrivere su standard error (stderr) in Haskell consente ai programmi di differenziare il loro output tra risultati normali e messaggi di errore. Questo è fondamentale per segnalare problemi e per il debugging, senza intasare lo standard output (stdout) che spesso trasporta i dati principali del programma o il risultato.

## Come fare:
In Haskell, scrivere su stderr è semplice con il modulo `System.IO` della libreria base. Qui sotto c'è un esempio base per dimostrare:

```haskell
import System.IO

main :: IO ()
main = do
  hPutStrLn stderr "Questo è un messaggio di errore."
```

L'output di questo programma su stderr sarebbe:

```
Questo è un messaggio di errore.
```

Se stai lavorando in un'applicazione più complessa, o se hai bisogno di un controllo migliore sul logging (inclusi gli errori), potresti optare per una libreria di terze parti. Una scelta popolare è `monad-logger` che si integra con lo stile di programmazione `mtl` di Haskell. Ecco un piccolo snippet usando `monad-logger`:

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Control.Monad.Logger

main :: IO ()
main = runStderrLoggingT $ do
  logErrorN "Questo è un messaggio di errore usando monad-logger."
```

Quando eseguito, la versione `monad-logger` emette in modo simile un messaggio di errore, ma è dotata di più contesto come timestamp o livelli di log, a seconda della configurazione:

```
[Error] Questo è un messaggio di errore usando monad-logger.
```

Entrambi i metodi servono allo scopo di scrivere su stderr, con la scelta che dipende in gran parte dalla complessità e dalle esigenze della tua applicazione.
