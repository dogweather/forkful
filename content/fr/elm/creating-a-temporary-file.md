---
title:                "Création d'un fichier temporaire"
html_title:           "Elm: Création d'un fichier temporaire"
simple_title:         "Création d'un fichier temporaire"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elm/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un développeur Elm, il est très probable que tôt ou tard, vous ayez besoin de créer un fichier temporaire. Que ce soit pour stocker des données temporaires ou pour tester une fonctionnalité, la création de fichiers temporaires peut être utile dans de nombreuses situations.

## Comment faire

Pour commencer, vous devrez utiliser une bibliothèque externe pour gérer la création de fichiers. La plus populaire et la plus bien documentée est la bibliothèque "JustinMimbs/file" disponible sur le site "package.elm-lang.org". Voici un exemple de code pour créer un fichier temporaire avec cette bibliothèque :

```Elm
file : Task String
file =
  Temp.file
    { filename = "temp_file.txt"
    , contents = "Contenu du fichier temporaire"
    }

```

Ce code crée un nouveau fichier temporaire appelé "temp_file.txt" et y écrit le contenu spécifié. La fonction "file" renvoie une "Task" qui peut être "performée" dans votre application. Voici un exemple d'utilisation :

```Elm
Task.perform (Debug.log "Fichier créé") file

```

Ce code exécute la "Task" et affiche le message "Fichier créé" dans la console. Une fois la tâche terminée, le fichier temporaire sera créé et disponible pour une utilisation ultérieure.

## Approfondissement

Il est important de noter que la fonction "file" ne crée pas réellement un fichier, elle génère une "Task" pour le créer. Ainsi, il est recommandé de l'exécuter immédiatement après l'avoir créée pour éviter tout problème de synchronisation des "Tasks". De plus, la bibliothèque "JustinMimbs/file" offre également d'autres fonctionnalités, telles que la suppression de fichiers temporaires et la manipulation de fichiers existants.

## Voir aussi

- [Bibliothèque "JustinMimbs/file"](https://package.elm-lang.org/packages/JustinMimbs/file/latest)
- [Documentation Elm sur la gestion des fichiers](https://elm-lang.org/docs/interop/file)