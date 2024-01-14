---
title:                "Haskell: Lire un fichier texte"
simple_title:         "Lire un fichier texte"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Pourquoi

La lecture de fichiers texte est une compétence importante pour tout programmeur Haskell. Cela vous permet d'importer et d'analyser des données externes dans votre programme, offrant ainsi une grande flexibilité et une utilité pratique. Dans cet article, nous explorerons comment lire un fichier texte en Haskell et partagerons quelques conseils utiles pour vous aider à maîtriser cette compétence.

## Comment faire

La première étape pour lire un fichier texte en Haskell consiste à l'ouvrir avec la fonction `openFile` de Haskell. Cette fonction prend deux arguments : le chemin d'accès au fichier et le mode d'ouverture (lecture, écriture, etc.). Une fois que le fichier est ouvert, nous pouvons utiliser la fonction `hGetContents` pour obtenir son contenu sous forme de chaîne de caractères.

Voici un exemple de code qui lit le contenu d'un fichier texte et l'affiche à l'écran :

```Haskell
import System.IO

main = do
    handle <- openFile "fichier.txt" ReadMode
    contents <- hGetContents handle
    putStrLn contents
    hClose handle
```

Lors de l'exécution de ce code, le contenu du fichier "fichier.txt" sera affiché à l'écran. Il est important de noter que la fonction `hGetContents` retourne une "lazy string", ce qui signifie qu'elle n'évalue pas le contenu du fichier tant que cela n'est pas nécessaire. Cela peut être très utile pour la manipulation de fichiers volumineux.

## Plongée en profondeur

En plus de la fonction `hGetContents`, Haskell offre de nombreuses autres fonctions pour la lecture et la manipulation de fichiers texte. Par exemple, `hGetLine` permet de lire ligne par ligne, tandis que `hGetChar` peut être utilisé pour lire caractère par caractère.

Il est également possible de lire un fichier texte au format binaire en utilisant la fonction `hGetBufSome` et de spécifier la taille du buffer de lecture. De plus, la bibliothèque de sérialisation de Haskell, "Data.Binary", peut être utilisée pour lire des données structurées à partir de fichiers binaires.

## Voir aussi

- [Documentation officielle de la bibliothèque standard de Haskell sur les entrées/sorties](https://downloads.haskell.org/~ghc/latest/docs/html/libraries/base/System-IO.html)
- [Tutoriel sur la lecture de fichiers avec Haskell](https://www.haskell.org/tutorial/io.html)
- [Documentation sur la bibliothèque "Data.Binary"](https://hackage.haskell.org/package/binary-0.8.5.1/docs/Data-Binary.html)