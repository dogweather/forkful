---
title:                "Lettura degli argomenti della riga di comando"
html_title:           "Java: Lettura degli argomenti della riga di comando"
simple_title:         "Lettura degli argomenti della riga di comando"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/haskell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Cos'è & Perché?

La lettura degli argomenti da riga di comando è un processo atto a catturare gli input inseriti in un terminale. Lo facciamo per permettere ai nostri programmi di interagire con l'utente in modo dinamico e personalizzato.

## Come si fa:

Ecco un semplice esempio che mostra come leggere gli argomenti da riga di comando in Haskell:

```Haskell
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    print args
```

Se avviate questo programma con `runhaskell MyProgram.hs arg1 arg2 arg3`, la stampa sarà:

```Haskell
["arg1", "arg2", "arg3"]
```

## Approfondimento

La lettura degli argomenti da riga di comando può essere modellata in molti modi. In Haskell, tipicamente si fa uso delle funzioni del modulo `System.Environment`. Alla sua origine, l'approccio segue una tradizione storica della programmazione Unix.

Un'alternativa comune in Haskell è usando il pacchetto `optparse-applicative`, che fornisce una serie di funzioni per la creazione di parser di argomenti da linea di comando.

Da un punto di vista realizzativo, `getArgs` in Haskell semplicemente accede all'array `argv` del C per ottenere tutti gli argomenti passati al programma. Questo approccio varia leggermente tra i diversi sistemi operativi, ma l'idea di base rimane la stessa.

## Vedi anche

Per ulteriori informazioni sulla lettura degli argomenti da linea di comando in Haskell, dai un'occhiata ai seguenti collegamenti:

- Documentazione di System.Environment: https://hackage.haskell.org/package/base-4.15.0.0/docs/System-Environment.html
- Pacchetto optparse-applicative: https://hackage.haskell.org/package/optparse-applicative
- Un approfondimento sulla linea di comando Unix: https://en.wikipedia.org/wiki/Command-line_interface#Arguments