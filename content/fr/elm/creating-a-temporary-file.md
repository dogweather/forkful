---
title:    "Elm: Créer un fichier temporaire"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Pourquoi

La création d'un fichier temporaire peut être utile pour stocker des données temporaires ou pour effectuer des opérations de manipulation de fichiers à court terme. Cela peut également être utilisé pour créer une sauvegarde de fichiers avant une mise à jour ou une modification importante.

## Comment faire

Pour créer un fichier temporaire en Elm, nous pouvons utiliser la fonction `File.create`. Cette fonction prend en paramètre le nom du fichier et renvoie un `Cmd Msg` qui peut être exécuté pour créer le fichier. Par exemple :

```Elm
File.create "mon_fichier.txt"
    |> Task.perform createFile
```

Ici, nous créons un fichier appelé "mon_fichier.txt" et exécutons ensuite la fonction `createFile` pour traiter le résultat. Vous pouvez également spécifier d'autres options telles que le chemin du fichier, les autorisations et les données à écrire dans le fichier.

## Plongée en profondeur

En utilisant la fonction `File.create`, nous pouvons également spécifier le format de données à écrire dans le fichier temporaire. Par exemple, si nous avons besoin d'écrire un fichier CSV, nous pouvons utiliser la fonction `File.createWith` qui prend en paramètre le nom du fichier ainsi qu'une fonction de formatage. Cette fonction de formatage peut être utilisée pour convertir nos données en une chaîne de caractères au format CSV avant de l'écrire dans le fichier.

## Voir aussi

- [Documentation officielle Elm pour la création de fichiers](https://package.elm-lang.org/packages/elm/file/latest/File#create)
- [Article sur la manipulation de fichiers en Elm](https://dev.to/kristianpedersen/working-with-files-in-elm-48f9)