---
title:    "Haskell: Lecture d'un fichier texte"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un développeur Haskell, il est fort probable que vous ayez à un moment donné besoin de lire un fichier texte dans votre programme. Que ce soit pour récupérer des données à partir d'un fichier externe ou pour traiter un fichier généré par votre propre programme, la lecture de fichiers texte est une compétence essentielle dans le monde de la programmation. Dans cet article, nous allons voir comment lire un fichier texte en utilisant Haskell.

## Comment faire

Dans Haskell, la première étape pour lire un fichier est d'ouvrir le fichier en utilisant la fonction `openFile`. Cette fonction prend deux arguments : le chemin du fichier et le mode d'ouverture (lecture, écriture, etc.). Voici un exemple de code pour ouvrir un fichier en lecture seule :

```Haskell
import System.IO

main = do
    file <- openFile "mon_fichier.txt" ReadMode
```

Une fois que le fichier est ouvert, nous pouvons utiliser la fonction `hGetContents` pour lire son contenu. Cette fonction prend un `Handle` (retourné par la fonction `openFile`) en paramètre et renvoie un `String` contenant tout le contenu du fichier.

```Haskell
import System.IO

main = do
    file <- openFile "mon_fichier.txt" ReadMode
    contents <- hGetContents file
    putStrLn contents -- affiche le contenu du fichier sur la console
```

Il est important de noter que la fonction `hGetContents` ne lit pas tout le contenu du fichier immédiatement, mais utilise un mécanisme de "lazy evaluation" pour ne lire que les parties du fichier demandées à mesure que nous en avons besoin.

## Plongée en profondeur

Si vous souhaitez lire un fichier ligne par ligne, plutôt que de tout lire en une seule fois, vous pouvez utiliser la fonction `hGetLine`. Cette fonction prend également un `Handle` en paramètre et renvoie une ligne du fichier sous forme de `String`.

```Haskell
import System.IO

main = do
    file <- openFile "mon_fichier.txt" ReadMode
    line <- hGetLine file -- lit une ligne du fichier
    putStrLn line -- affiche la ligne sur la console
```

Si vous avez besoin de lire des valeurs numériques à partir du fichier, vous pouvez utiliser la fonction `hGetContents` avec la fonction `read`. Cela convertira le `String` lu à partir du fichier en valeur numérique.

```Haskell
import System.IO

main = do
    file <- openFile "mon_fichier.txt" ReadMode
    contents <- hGetContents file
    let num = read contents :: Int -- convertit le contenu en Int
    print (3 * num) -- affiche le résultat de la multiplication sur la console
```

Il est également important de s'assurer de fermer le fichier après l'avoir lu en utilisant la fonction `hClose`. Cela permettra de libérer les ressources utilisées par le `Handle`.

```Haskell
import System.IO

main = do
    file <- openFile "mon_fichier.txt" ReadMode
    contents <- hGetContents file
    putStrLn contents -- affiche le contenu du fichier sur la console
    hClose file -- ferme le fichier
```

## Voir aussi

Vous pouvez trouver plus d'informations sur la lecture de fichiers en Haskell sur ces sites :

- [Documentation officielle de Haskell sur la gestion des fichiers](https://haskell.org/haskellwiki/Handling_files)
- [Tutoriel sur la lecture et l'écriture de fichiers en Haskell](https://www.tutorialspoint.com/haskell/haskell_input_output.htm)
- [Exemple de code pour lire un fichier texte en Haskell](https://rosettacode.org/wiki/Read_entire_file#Haskell)