---
title:                "Création d'un fichier temporaire"
html_title:           "Kotlin: Création d'un fichier temporaire"
simple_title:         "Création d'un fichier temporaire"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elm/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et Pourquoi?

Créer un fichier temporaire est une tâche nœud-jour communément accomplie par les programmeurs. Cela aide à stocker et manipuler des données de manière éphémère et sans perturber les opérations quotidiennes. 

## Comment faire:

Elm n'a pas directement accès au système de fichiers, donc nous allons utiliser une tâche à travers JavaScript, en utilisant des ports:

```Elm
port module TempFilePorts exposing (..)

port saveTempFile : String -> Cmd msg

port saveTempFileReceived : (Result String String -> msg) -> Sub msg
```

Dans JavaScript, nous pouvons créer une fonction pour les ports qui gère le travail:

```JavaScript
app.ports.saveTempFile.subscribe(function(data) {
  // Ici, vous pouvez utiliser une bibliothèque ou une API pour créer un fichier temporaire
  // et y enregistrer des données.
  // Une fois que vous avez terminé, envoyez le chemin du fichier ou une erreur à Elm:

  app.ports.saveTempFileReceived.send({ tag: "ok", data: tempFilePath });
});
```

## Plongée profonde

Créer un fichier temporaire est une partie importante de nombreux systèmes depuis les débuts de l'informatique. En Elm, la manipulation du système de fichiers n'est pas intégrée car Elm est principalement destiné au développement Web et donc sans accès direct au système de fichiers pour des raisons de sécurité.

Alternativement, vous pouvez déplacer la logique de votre programme impliquant des fichiers temporaires vers une API côté serveur si vous avez le contrôle sur cela.

Notez que si vous utilisez une bibliothèque ou une API pour créer le fichier temporaire, vous devez vous assurer que le fichier est bien supprimé après utilisation.

## Voir aussi

- [Documentation officielle Elm](https://guide.elm-lang.org/)
- [Article à lire: Elm et JavaScript Interop](https://elmprogramming.com/interop.html)
- [Guide pour les ports Elm](https://elmprogramming.com/ports.html)