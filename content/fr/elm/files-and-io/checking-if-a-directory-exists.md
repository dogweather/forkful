---
date: 2024-01-19
description: "How to: \"Comment Faire :\" Elm n'est pas con\xE7u pour interagir directement\
  \ avec le syst\xE8me de fichiers. Il faut faire appel \xE0 JavaScript par l'interm\xE9\
  diaire\u2026"
lastmod: '2024-03-13T22:44:57.705209-06:00'
model: unknown
summary: "\"Comment Faire :\"\n\nElm n'est pas con\xE7u pour interagir directement\
  \ avec le syst\xE8me de fichiers."
title: "V\xE9rification de l'existence d'un r\xE9pertoire"
weight: 20
---

## How to:
"Comment Faire :"

Elm n'est pas conçu pour interagir directement avec le système de fichiers. Il faut faire appel à JavaScript par l'intermédiaire des ports. Voici comment on peut s'organiser :

```Elm
port module DirectoryExists exposing (..)

-- Définir un port pour envoyer le chemin à vérifier
port checkDirectory : String -> Cmd msg

-- Définir un port pour recevoir la réponse
port directoryExists : (Bool -> msg) -> Sub msg

-- Exemple d'abonnement à la réponse
subscriptions : Model -> Sub Msg
subscriptions model =
    directoryExists DirectoryExistsResponse
```

Puis, dans JavaScript, vous utiliseriez l'API `fs` de Node.js pour vérifier si un dossier existe :

```javascript
const { app } = require('elm');
const fs = require('fs');

app.ports.checkDirectory.subscribe((path) => {
    fs.access(path, fs.constants.F_OK, (err) => {
        app.ports.directoryExists.send(!err);
    });
});
```

Le port `directoryExists` renverrait `True` si le dossier existe, sinon `False`.

## Deep Dive:
"Plongée en Profondeur" : Elm vise la sécurité et la fiabilité dans les applications web. Il n'a pas d'accès natif au système de fichiers, ce qui est une tâche courante en Node.js, mais pas dans un navigateur. Les ports permettent une communication indolore avec JavaScript, qui lui peut interagir avec le système de fichiers. La décision d'Elm d'isoler ces effets secondaires assure une architecture plus prédictive et sécurisée.

## See Also:
"Voir Aussi" :

- [Elm Ports](https://guide.elm-lang.org/interop/ports.html)
- [Node.js fs documentation](https://nodejs.org/api/fs.html)
