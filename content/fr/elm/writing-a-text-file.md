---
title:                "Elm: Écrire un fichier texte"
simple_title:         "Écrire un fichier texte"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elm/writing-a-text-file.md"
---

{{< edit_this_page >}}

# Pourquoi écrire un fichier texte en Elm ?

Si vous êtes un programmeur Elm, vous savez probablement déjà que ce langage de programmation fonctionnel est parfait pour créer des applications Web réactives. Mais saviez-vous qu'il est également un excellent choix pour écrire des fichiers texte ? Dans cet article, nous allons explorer pourquoi vous pourriez vouloir écrire des fichiers texte en Elm et comment le faire.

## Comment écrire un fichier texte en Elm

L'écriture d'un fichier texte en Elm est assez simple. Vous pouvez utiliser la fonction `File.writeFile` du module `File` pour créer un fichier texte à partir d'une chaîne de caractères. Par exemple :

```Elm
import File

myString = "Bonjour le monde !"

File.writeFile "monFichier.txt" myString
```

Dans cet exemple, nous avons utilisé `File.writeFile` pour créer un fichier texte appelé "monFichier.txt" et lui avons assigné la chaîne de caractères "Bonjour le monde !" en tant que contenu. Cette fonction renvoie un résultat de type `Cmd msg` qui peut être utilisé pour envoyer une commande à l'architecture Elm.

## Plongée profonde

Écrire un simple fichier texte peut sembler facile, mais il y a certaines choses à garder à l'esprit lorsque vous travaillez avec des fichiers en Elm. Par exemple, vous pouvez utiliser `File.writeAppend` pour ajouter du contenu à un fichier existant plutôt que de le remplacer complètement. Vous pouvez également utiliser `File.read` pour lire le contenu d'un fichier et `File.delete` pour le supprimer.

## Voir aussi

- [Documentation officielle Elm pour le module File](https://package.elm-lang.org/packages/elm/file/latest/File)
- [Exemple de code pour écrire un fichier texte en Elm](https://github.com/elm-lang/elm-guide/issues/366)

Maintenant que vous savez comment écrire un fichier texte en Elm, vous pouvez l'ajouter à votre boîte à outils de programmeur. N'hésitez pas à expérimenter avec les différentes fonctions disponibles et à les utiliser pour créer des fichiers texte à des fins diverses. Bonne programmation !