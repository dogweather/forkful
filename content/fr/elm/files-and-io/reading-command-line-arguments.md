---
date: 2024-01-20 17:55:46.536761-07:00
description: "How to: Elm n'est pas con\xE7u pour cr\xE9er des applications CLI, donc\
  \ lire directement les arguments de la ligne de commande n'est pas possible. Mais\
  \ on peut\u2026"
lastmod: '2024-03-13T22:44:57.706164-06:00'
model: gpt-4-1106-preview
summary: "Elm n'est pas con\xE7u pour cr\xE9er des applications CLI, donc lire directement\
  \ les arguments de la ligne de commande n'est pas possible."
title: Lecture des arguments de ligne de commande
weight: 23
---

## How to:
Elm n'est pas conçu pour créer des applications CLI, donc lire directement les arguments de la ligne de commande n'est pas possible. Mais on peut intégrer Elm dans une application Node.js, par exemple, pour le faire. Voici comment utiliser Elm avec `Process.argv` de Node.js :

```javascript
// index.js
const { Elm } = require('./Main');
const app = Elm.Main.init({
  flags: process.argv.slice(2)
});

// Assume we send the arguments to Elm for processing
app.ports.output.subscribe(function(message) {
  console.log("Elm says:", message);
});
```

```elm
-- Main.elm
port module Main exposing (..)

import Platform

port output : String -> Cmd msg

main =
    Platform.worker
        { init = init
        , update = \_ model -> (model, Cmd.none)
        , subscriptions = \_ -> Sub.none
        }

init : Flags -> ( Model, Cmd msg )
init flags =
    ( Model, output ("Arguments: " ++ String.join ", " flags) )
```

Lancez avec `node index.js arg1 arg2`. La sortie sera : `Elm says: Arguments: arg1, arg2`

## Deep Dive
Elm est plutôt orienté vers les applications web. Il ne propose pas de fonctionnalité intégrée pour les entrées CLI comme d'autres langages (Node.js, Python...). Historiquement, Elm se concentre sur la sûreté et l'expérience front-end, ce qui explique pourquoi les entrées CLI ne sont pas dans sa zone. Pour contourner cela, on utilise souvent des ports pour interagir avec JavaScript, qui s'occupe de cette tâche.

Alternatives? Bien sûr, si vous avez besoin de CLI en Elm, c'est sans doute signe qu'il faut chercher ailleurs; ou alors, créer un wrapper en Node.js comme montré ci-dessus.

Pour l'intégration, on s'appuie sur Node.js (ou autre solution côté serveur) pour passer les arguments à Elm via des ports, qui sont des points d'échange entre Elm et JavaScript.

## See Also
- Elm Ports: https://guide.elm-lang.org/interop/ports.html
- Node.js `process.argv` documentation: https://nodejs.org/docs/latest/api/process.html#process_process_argv
- An example Elm/Node.js CLI project: https://github.com/dillonkearns/elm-cli-options-parser
