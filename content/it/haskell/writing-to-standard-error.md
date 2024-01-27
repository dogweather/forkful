---
title:                "Scrivere sull'errore standard"
date:                  2024-01-19
html_title:           "Arduino: Scrivere sull'errore standard"
simple_title:         "Scrivere sull'errore standard"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/haskell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why?
Scrivere su standard error (stderr) permette di separare i messaggi di errore dall'output standard (stdout). I programmatori fanno ciò per facilitare il debug e gestire correttamente i flussi di output.

## How to:
Haskell usa `hPutStr` e `hPutStrLn` con `stderr` per scrivere su standard error. Ecco come farlo.

```Haskell
import System.IO (stderr, hPutStr, hPutStrLn)

main :: IO ()
main = do
  hPutStrLn stderr "Questo è un messaggio di errore."
```

Output dell’esempio:
```
Questo è un messaggio di errore.
```

## Deep Dive
Historical context: `stderr` è assieme a `stdout` fin dall’inizio dei sistemi operativi Unix-like, separando errore e output per convenzione.
Alternatives: In alternativa, puoi usare librerie di terze parti per logging avanzato, come `monad-logger`.
Implementation details: Scrivere su `stderr` in Haskell è gestito dal pacchetto `base`, tramite il modulo `System.IO`.

## See Also
- Haskell Documentation: https://www.haskell.org/documentation/
- `System.IO` library: https://hackage.haskell.org/package/base/docs/System-IO.html
- `monad-logger`: https://hackage.haskell.org/package/monad-logger
