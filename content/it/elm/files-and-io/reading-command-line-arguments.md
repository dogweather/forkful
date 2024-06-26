---
date: 2024-01-20 17:55:50.278043-07:00
description: "How to: Elm \xE8 principalmente focalizzato sullo sviluppo web e non\
  \ fornisce accesso diretto agli argomenti da riga di comando come farebbe un linguaggio\u2026"
lastmod: '2024-03-13T22:44:43.364677-06:00'
model: gpt-4-1106-preview
summary: "Elm \xE8 principalmente focalizzato sullo sviluppo web e non fornisce accesso\
  \ diretto agli argomenti da riga di comando come farebbe un linguaggio come Python\
  \ o Node.js."
title: Lettura degli argomenti della riga di comando
weight: 23
---

## How to:
Elm è principalmente focalizzato sullo sviluppo web e non fornisce accesso diretto agli argomenti da riga di comando come farebbe un linguaggio come Python o Node.js. Tuttavia, possiamo interagire con JavaScript per ottenere valori dalla riga di comando usando ports.

```Elm
port module Main exposing (..)

-- In Elm, definisci una `port` per inviare i dati a JavaScript
port cmdlineArgs : (List String -> msg) -> Sub msg

-- In JavaScript, inviare l'array di argumenti a Elm
app.ports.cmdlineArgs.send(process.argv);
```
Dato che Elm non è progettato per script CLI, non ci sarà output diretto da Elm, ma potrai vedere gli effetti nella tua app web o attraverso messaggi console in JS.

## Deep Dive
Elm, creato da Evan Czaplicki nel 2012, fu pensato per migliorare la sicurezza e la facilità nello sviluppo di applicazioni web, quindi non include funzionalità native per la manipolazione degli argomenti della riga di comando. Nel contesto dei sistemi CLI, sarebbe più appropriato usare altri linguaggi come Haskell o PureScript, che hanno una sintassi funzionale simile ma sono più adatti a tale scopo.

In Elm, se devi affrontare il bisogno di leggere argomenti da riga di comando, il modo ufficiale è tramite l'interoperabilità con JavaScript — i ports. Questi sono canali bidirezionali: Elm può inviare richieste a JavaScript e JavaScript può inviare dati a Elm.

Ad oggi non ci sono alternative native in Elm, quindi l'utilizzo di JavaScript è il modo d'andare. È bene, però, limitare l'uso di JavaScript per mantenere i benefici della sicurezza e della prevedibilità del codice Elm.

## See Also
- Elm Ports: https://guide.elm-lang.org/interop/ports.html
- Documentazione Elm: https://elm-lang.org/docs
- Esempi di interoperabilità Elm e JavaScript: https://elm-lang.org/examples/ports
