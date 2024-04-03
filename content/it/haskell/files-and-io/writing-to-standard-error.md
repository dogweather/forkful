---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:33:14.636659-07:00
description: "Come fare: In Haskell, scrivere su stderr \xE8 semplice con il modulo\
  \ `System.IO` della libreria base. Qui sotto c'\xE8 un esempio base per dimostrare."
lastmod: '2024-03-13T22:44:43.491645-06:00'
model: gpt-4-0125-preview
summary: "In Haskell, scrivere su stderr \xE8 semplice con il modulo `System.IO` della\
  \ libreria base."
title: Scrivere sull'errore standard
weight: 25
---

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
