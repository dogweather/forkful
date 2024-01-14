---
title:                "Elm: Lire un fichier texte"
simple_title:         "Lire un fichier texte"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elm/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un développeur Elm, vous êtes probablement familiarisé avec le processus de lecture de fichiers de code pour y accéder et les modifier. Cependant, la lecture de fichiers texte peut sembler plus compliquée et moins utile. Dans cet article, nous allons discuter de l'importance de la lecture de fichiers texte et comment le faire en utilisant Elm.

## Comment faire

La première étape pour lire un fichier texte en Elm est d'utiliser la fonction `file` de la bibliothèque `elm/file`. Cette fonction prend en paramètre le nom du fichier que vous souhaitez lire et renvoie un `Result` avec le contenu du fichier s'il est réussi, ou une erreur si le fichier ne peut pas être trouvé ou lu.

Un exemple de code pour lire un fichier texte nommé "data.txt" et afficher son contenu dans la console serait :

```Elm 
file "data.txt"
    |> Task.attempt handleFileResult

handleFileResult : Result String String -> Cmd msg
handleFileResult result =
    case result of
        Ok content ->
            content
                |> Debug.log "Contenu du fichier"

        Err error ->
            error
                |> Debug.log "Erreur lors de la lecture du fichier"
```

En utilisant cette fonction, vous pouvez facilement accéder au contenu d'un fichier texte et l'utiliser dans votre programme Elm.

## Plongée en profondeur

La fonction `file` peut également être utilisée pour lire des fichiers dans un format spécifique, tels que le JSON. En utilisant la fonction `Decode.decodeString` de la bibliothèque `elm/json`, vous pouvez convertir le contenu du fichier en données utilisables dans votre programme.

De plus, n'oubliez pas que la lecture de fichiers peut également être utilisée pour créer des applications plus interactives, telles que des jeux ou des quiz, en utilisant des fichiers texte pour stocker les questions, les réponses et plus encore.

## Voir aussi

- Documentation sur la fonction `file` : https://package.elm-lang.org/packages/elm/file/latest/File#file
- Tutoriel sur la lecture de fichiers en Elm : https://elmprogramming.com/reading-files-in-elm.html
- Exemple de projet utilisant la lecture de fichiers en Elm : https://github.com/leifmettendorf/elm-text-file-reader