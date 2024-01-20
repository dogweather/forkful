---
title:                "Écrire un fichier texte"
html_title:           "Elm: Écrire un fichier texte"
simple_title:         "Écrire un fichier texte"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elm/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Qu'est-ce que & Pourquoi?
Ecrire un fichier texte signifie simplement créer un document contenant du texte lisible par un ordinateur. Les programmeurs utilisent cette technique pour stocker des informations telles que des données de configuration ou des messages pouvant être affichés dans une application.

## Comment faire:
```Elm
-- Créer un nouveau fichier texte
textFile : String -> Cmd msg
textFile name =
    File.writeString name "Contenu du fichier texte"
```
Pour créer un fichier texte en Elm, il suffit d'utiliser la fonction ```File.writeString``` et de fournir un nom de fichier ainsi que le contenu que vous souhaitez y écrire. Vous pouvez ensuite le tester en utilisant la fonction ```Cmd.map```.

## Plongeon profond:
Il est important de noter que la capacité d'écrire des fichiers texte en Elm n'est pas disponible dans tous les navigateurs. Cela signifie que cette technique peut ne pas être adaptée à toutes les applications web. Cependant, il existe d'autres options telles que l'utilisation de bases de données en ligne ou de services tiers pour stocker vos informations.

## Voir aussi:
- [Article sur la gestion des fichiers en Elm](https://medium.com/@spielhuus/working-with-files-in-elm-4c686054ca24)