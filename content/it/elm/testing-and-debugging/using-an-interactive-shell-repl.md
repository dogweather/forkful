---
date: 2024-01-26 04:13:31.996309-07:00
description: "Come fare: Elm non viene fornito con un REPL integrato. Tuttavia, puoi\
  \ utilizzare `elm repl` dalla tua riga di comando per avviare una sessione Elm dopo\u2026"
lastmod: '2024-03-13T22:44:43.351417-06:00'
model: gpt-4-0125-preview
summary: Elm non viene fornito con un REPL integrato.
title: Utilizzo di un interprete interattivo (REPL)
weight: 34
---

## Come fare:
Elm non viene fornito con un REPL integrato. Tuttavia, puoi utilizzare `elm repl` dalla tua riga di comando per avviare una sessione Elm dopo aver installato Elm.

```Elm
> import List exposing (..)
> map (\x -> x * 2) [1, 2, 3, 4]
[2,4,6,8] : List number
```

In questa sessione, dopo aver importato le funzioni di List, abbiamo raddoppiato i numeri in un elenco e ottenuto il risultato istantaneamente.

## Approfondimento
Il REPL di Elm può sembrare limitato rispetto a quelli di altri linguaggi come Python o JavaScript, poiché Elm è un linguaggio compilato focalizzato sulla produzione di applicazioni web. Storicamente, Elm si è concentrato su applicazioni complete piuttosto che su scripting o interazioni via shell.

Le alternative al REPL di Elm includono `elm-live` e editor online come Ellie, dove puoi vedere le modifiche al codice riflesse in tempo reale in un browser.

Per quanto riguarda l'implementazione, il REPL di Elm compila frammenti di codice Elm in JavaScript in background, permettendoti di eseguire Elm in modo interattivo. Questo è differente dai REPL di linguaggi interpretati, che non necessitano di questo passaggio di compilazione. Il REPL di Elm è anche semplificato per mantenere il linguaggio core leggero e focalizzato.

## Vedi Anche
- La guida ufficiale di Elm sull'interattività: https://guide.elm-lang.org/interop/
- Ellie, un parco giochi online per Elm: https://ellie-app.com/new
- `elm-live`, un server di sviluppo flessibile per Elm: https://www.elm-live.com/
