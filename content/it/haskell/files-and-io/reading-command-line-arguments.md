---
date: 2024-01-20 17:56:30.018207-07:00
description: "Leggere gli argomenti da riga di comando significa estrarre le informazioni\
  \ passate al tuo programma quando viene eseguito da terminale. \xC8 cruciale per\u2026"
lastmod: 2024-02-19 22:05:02.556101
model: gpt-4-1106-preview
summary: "Leggere gli argomenti da riga di comando significa estrarre le informazioni\
  \ passate al tuo programma quando viene eseguito da terminale. \xC8 cruciale per\u2026"
title: Lettura degli argomenti della riga di comando
---

{{< edit_this_page >}}

## Cosa & Perché?
Leggere gli argomenti da riga di comando significa estrarre le informazioni passate al tuo programma quando viene eseguito da terminale. È cruciale per personalizzare l'esecuzione del software senza cambiare il codice.

## Come Fare:
Con Haskell, utilizziamo il modulo `System.Environment` per afferrare questi argomenti. Ecco un esempio semplice:

```haskell
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    print args
```

Se esegui questo programma così `runhaskell myprogram.hs arg1 arg2`, otterrai:

```plaintext
["arg1", "arg2"]
```

Per un uso pratico, guardiamo un esempio dove sommiamo numeri passati come argomenti:

```haskell
import System.Environment (getArgs)
import Text.Read (readMaybe)

main :: IO ()
main = do
    args <- getArgs
    let nums = mapM readMaybe args :: Maybe [Int]
    case nums of
        Just numbers -> print $ sum numbers
        Nothing -> putStrLn "Per favore, inserisci solo numeri."
```

Se lanci `runhaskell sum.hs 1 2 3`, otterrai `6`.

## Approfondimento:
Haskell legge gli argomenti da riga di comando tramite il modulo `System.Environment`. Vecchie versioni di Haskell avevano approcci diversi, ma questo è diventato lo standard.

Esistono alternative:

- `getArgs` restituisce una lista di stringhe. Per qualcosa di più robusto, si usa `optparse-applicative` o `GetOpt`.
- Per applicazioni complesse, `optparse-applicative` fornisce un'interfaccia tipo DSL per definire le opzioni.

La gestione degli argomenti in Haskell è puramente funzionale ed è una pratica comune in applicazioni come script e tool CLI.

## Vedi Anche:
- Documentazione di Haskell `System.Environment`: https://hackage.haskell.org/package/base/docs/System-Environment.html
- Documentazione di "optparse-applicative": https://hackage.haskell.org/package/optparse-applicative
- Haskell Wiki per l'analisi degli argomenti da riga di comando: https://wiki.haskell.org/Command_line_option_parsers
