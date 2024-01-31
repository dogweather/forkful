---
title:                "Vérification de l'existence d'un répertoire"
date:                  2024-01-19
html_title:           "Bash: Vérification de l'existence d'un répertoire"
simple_title:         "Vérification de l'existence d'un répertoire"

category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elm/checking-if-a-directory-exists.md"
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
