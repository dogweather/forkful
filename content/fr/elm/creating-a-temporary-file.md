---
title:    "Elm: Création d'un fichier temporaire"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/elm/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Pourquoi

Dans cet article, nous allons explorer comment créer des fichiers temporaires en Elm. Les fichiers temporaires peuvent être utiles pour stocker des données temporaires ou pour effectuer des tâches de manipulation de fichiers. Nous allons voir pourquoi cela peut être utile, comment le faire et plonger plus en profondeur dans cette technique.

## Comment faire

Pour créer un fichier temporaire, nous allons utiliser la fonction `File.Temp.file` du package `elm/file`. Voici un exemple de code :

```Elm
import File.Temp

main =
    File.Temp.file "test.txt" "Hello, world!"
        |> Task.attempt handleResult

handleResult result =
    case result of
        Ok path ->
            Debug.log "The path to the temporary file is:" path
            -- Output: The path to the temporary file is: /var/folders/23/stcmfnbp4dx_ey9r1m4hpkrm0000gn/T/elm-temp-9t9gN4k/test.txt

        Err error ->
            Debug.log "Error creating temporary file:" error

```

Dans cet exemple, nous créons un fichier temporaire nommé "test.txt" avec le contenu "Hello, world!". Nous utilisons `Task.attempt` pour gérer le résultat de la création du fichier, qui peut être soit un `Ok` avec le chemin du fichier temporaire, soit une `Err` avec une erreur.

## Plongée en profondeur

Il est important de noter que la fonction `File.Temp.file` crée un fichier temporaire qui sera automatiquement supprimé lorsque l'application Elm se termine. Cela signifie que le fichier temporaire ne persistera pas entre les exécutions de l'application. De plus, en utilisant `Task.attempt`, nous pouvons gérer les erreurs lors de la création du fichier temporaire, par exemple si l'utilisateur n'a pas les permissions nécessaires pour créer des fichiers.

Pour plus d'informations sur les fichiers temporaires en Elm, vous pouvez consulter la documentation du package `elm/file` <https://package.elm-lang.org/packages/elm/file/latest/>. Vous pouvez également explorer d'autres fonctions utiles telles que `File.Temp.directory`, qui crée un répertoire temporaire, ou `File.Temp.with`, qui nous permet de faire une opération avec un fichier temporaire et de le supprimer automatiquement après.

## Voir aussi

- Documentation du package `elm/file` <https://package.elm-lang.org/packages/elm/file/latest/>
- Tutoriel sur les fichiers en Elm <https://elmprogramming.com/working-with-files-in-elm.html>
- Exemples de code pour la manipulation de fichiers en Elm <https://github.com/elm-community/elm-file/tree/master/examples>