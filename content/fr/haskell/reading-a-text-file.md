---
title:                "Lecture d'un fichier texte"
html_title:           "Arduino: Lecture d'un fichier texte"
simple_title:         "Lecture d'un fichier texte"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et Pourquoi?

La lecture d'un fichier texte est l'action d'extraire de l'information contenue dans un fichier texte. Les programmeurs ont souvent besoin de le faire pour analyser les données, faire du traitement de texte, déboguer, et plus encore.

## Comment faire:

Dans Haskell, la fonction `readFile` permet de lire un fichier texte. 

```
import System.IO

main :: IO ()
main = do  
    contenu <- readFile "sample.txt"
    putStr contenu
```

Cette opération imprime le contenu du fichier `sample.txt`.

## Deep Dive 

Haskell, qui a débuté vers 1990, est un langage de programmation fonctionnelle dont l'idee de base est de manipuler les fichiers paresseux, c'est-à-dire de ne lire les données qu'au moment où elles sont nécessaires.

Il existe des alternatives à `readFile`, comme `hGetContents` qui prend un `Handle` sur un fichier ouvert, plutôt qu'un nom de fichier. 

Pour ajouter plus de contrôle, on peut aussi utiliser le module `Data.ByteString` avec les fonctions `readFile` et `hGetContents` pour lire les données sous forme de `ByteString`. Cependant, cela nécessite un peu plus de manipulation pour convertir les données en `String`.

## Voir également

Si vous souhaitez approfondir la lecture des fichiers en Haskell, vous pouvez consulter ces ressources:

- https://hackage.haskell.org/package/base-4.15.0.0/docs/Prelude.html#v:readFile
- https://wiki.haskell.org/How_to_work_with_files_and_directories
- https://haskell-lang.org/tutorial/io
- https://learnxinyminutes.com/docs/fr-fr/haskell-fr/