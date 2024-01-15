---
title:                "Ecrire un fichier texte"
html_title:           "Elm: Ecrire un fichier texte"
simple_title:         "Ecrire un fichier texte"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elm/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous aimez la programmation fonctionnelle et les langages statiquement typés, alors vous allez adorer Elm. Avec sa syntaxe simple et son système de types strict, écrire du code en Elm est un plaisir. Dans cet article, nous allons nous concentrer sur l'écriture de fichiers texte en utilisant Elm, afin que vous puissiez ajouter cette compétence utile à votre arsenal de programmation.

## Comment faire

Ecrire un fichier texte en Elm est un processus simple qui peut être réalisé en quelques étapes.

Tout d'abord, nous devons importer le module `File` de la bibliothèque standard d'Elm. Cela peut être fait en ajoutant la ligne suivante au début de votre fichier Elm :

```
import File
```

Ensuite, nous avons besoin d'une fonction qui prendra en charge la création et l'écriture du fichier. Nous l'appellerons `createAndWriteFile` et elle aura deux paramètres : le nom du fichier et le contenu que nous voulons écrire dans ce fichier. Voici à quoi ressemble la fonction complète :

```
createAndWriteFile : String -> String -> Cmd msg
createAndWriteFile fileName content =
  File.write fileName content
```

En exécutant cette fonction avec les arguments appropriés, un fichier texte contenant le contenu spécifié sera créé. Cependant, nous devons également gérer les possibles erreurs lors de l'écriture du fichier. Elm propose un type de données `Result` pour gérer ces erreurs et nous pouvons l'utiliser pour améliorer notre fonction `createAndWriteFile` :

```
createAndWriteFile : String -> String -> Cmd (Result File.Error ())
createAndWriteFile fileName content =
  File.write fileName content
```

Enfin, nous devons appeler `createAndWriteFile` en lui passant le nom du fichier et le contenu que nous voulons écrire. Nous devons également manipuler le résultat retourné pour traiter les erreurs, en utilisant la fonction `Cmd.map` et en fournissant une fonction qui sera appelée en cas de succès ou d'échec de l'écriture du fichier. Voici un exemple complet de comment écrire un fichier texte en Elm :

```
import File

createAndWriteFile : String -> String -> Cmd (Result File.Error ())
createAndWriteFile fileName content =
  File.write fileName content

handleResult : Result File.Error () -> String
handleResult result =
  case result of
    Ok _ -> "Fichier créé et contenu écrit avec succès !"
    Err error -> "Erreur lors de la création ou de l'écriture du fichier : " ++ File.Error.toString error

main : Program () Model Msg
main =
  let
    fileName = "monFichier.txt"
    content = "Bonjour le monde !"
    command = createAndWriteFile fileName content
  in
    Html.text (handleResult (Cmd.attempt command)) 
```

Vous pouvez exécuter ce code dans [Ellie](https://ellie-app.com/) et voir le résultat de la fonction `handleResult`.

## Plongée en profondeur

Maintenant que vous savez comment écrire un fichier texte en Elm, il est important de comprendre comment cette fonctionnalité fonctionne en coulisses.

L'utilisation du module `File` nécessite le consentement explicite de l'utilisateur, car cela implique des interactions avec le système de fichiers de l'ordinateur de l'utilisateur. Cela signifie que l'utilisation du module `File` n'est pas prise en charge lors de l'exécution de code dans votre navigateur, mais seulement lors de l'exécution de code sur un serveur ou en utilisant un package Elm spécifique pour les interactions avec le système de fichiers du navigateur.

Avec Elm, la gestion des erreurs est facile grâce au type de données `Result`. Cela permet de gérer proprement les erreurs lors de l'écriture de fichiers et de fournir un retour d'information à l'utilisateur en cas d'échec.

## Voir aussi

- [Documentation officielle d'Elm sur le module `File`](https://package.elm-lang.org/packages/elm/file/latest/)
- [Ellie App pour compiler et exécuter du code Elm en ligne](https://ellie-app.com/)
- [Elm in Action : un livre complet sur la programmation en Elm](https://www.manning.com/books/elm-in-action)