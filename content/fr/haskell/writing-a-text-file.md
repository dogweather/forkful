---
title:                "Écriture d'un fichier texte"
html_title:           "Haskell: Écriture d'un fichier texte"
simple_title:         "Écriture d'un fichier texte"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/writing-a-text-file.md"
---

{{< edit_this_page >}}

# Pourquoi

Ecrire un fichier texte en Haskell peut sembler intimidant pour les débutants, mais c'est en fait une compétence très utile à maîtriser. Non seulement cela vous aidera à mieux comprendre la syntaxe de ce langage de programmation fonctionnel, mais cela vous permettra également de créer des applications plus robustes et plus efficaces.

# Comment faire

### Lecture de fichiers texte

```Haskell
main = do
  let path = "mon_fichier.txt"
  contenu <- readFile path
  putStrLn contenu
```

Dans cet exemple, nous définissons d'abord le chemin d'accès de notre fichier texte en utilisant "let". Ensuite, nous utilisons la fonction "readFile" pour lire le contenu du fichier et le stocker dans une variable appelée "contenu". Enfin, nous utilisons la fonction "putStrLn" pour afficher le contenu à l'écran.

### Ecrire dans un fichier texte

```Haskell
main = do
  let path = "mon_fichier.txt"
  let contenu = "Voici le contenu de mon fichier."
  writeFile path contenu
```

Dans cet exemple, nous créons un fichier texte appelé "mon_fichier.txt" et y écrivons le contenu "Voici le contenu de mon fichier." en utilisant la fonction "writeFile".

# Plongée en profondeur

Il est important de noter que ces exemples utilisent les fonctions "readFile" et "writeFile" de la bibliothèque "System.IO". Il existe cependant d'autres façons d'écrire et de lire des fichiers en Haskell, en utilisant par exemple la bibliothèque "Data.Text" ou en utilisant des opérateurs de haute performance comme "ByteString". Il est également important de manipuler correctement les erreurs lors de la lecture et de l'écriture de fichiers en utilisant des fonctions telles que "catch" et "hPutStrLn".

# Voir aussi

- https://wiki.haskell.org/Handling_IO#Reading_a_File
- https://wiki.haskell.org/Handling_IO#Writing_CSV_Files