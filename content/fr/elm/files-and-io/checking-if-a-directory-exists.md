---
aliases:
- /fr/elm/checking-if-a-directory-exists/
date: 2024-01-19
description: "\"Quoi et Pourquoi ?\": V\xE9rifier si un dossier existe c'est s'assurer\
  \ que le chemin d'acc\xE8s pointe vers quelque chose de r\xE9el. Les d\xE9veloppeurs\
  \ font \xE7a pour\u2026"
lastmod: 2024-02-18 23:09:08.750328
summary: "\"Quoi et Pourquoi ?\": V\xE9rifier si un dossier existe c'est s'assurer\
  \ que le chemin d'acc\xE8s pointe vers quelque chose de r\xE9el. Les d\xE9veloppeurs\
  \ font \xE7a pour\u2026"
title: "V\xE9rification de l'existence d'un r\xE9pertoire"
---

{{< edit_this_page >}}

## What & Why?
"Quoi et Pourquoi ?": Vérifier si un dossier existe c'est s'assurer que le chemin d'accès pointe vers quelque chose de réel. Les développeurs font ça pour éviter des erreurs en tentant d'accéder ou de modifier un dossier inexistant.

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
