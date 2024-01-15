---
title:                "Lecture d'un fichier texte"
html_title:           "Haskell: Lecture d'un fichier texte"
simple_title:         "Lecture d'un fichier texte"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes curieux de la programmation fonctionnelle ou si vous souhaitez simplement découvrir de nouveaux langages de programmation, la lecture d'un fichier texte en Haskell est un bon point de départ. Cela vous permettra de comprendre les bases du langage et de voir comment Haskell gère la lecture et l'écriture de fichiers.

## Comment faire

La première étape pour lire un fichier texte en Haskell est d'importer le module "System.IO". Ensuite, vous devez utiliser la fonction "openFile" pour ouvrir le fichier en mode lecture, en spécifiant le chemin d'accès et le mode dans lequel vous souhaitez ouvrir le fichier. Voici un exemple de code :

```Haskell
import System.IO

main = do
    file <- openFile "monfichier.txt" ReadMode
    contents <- hGetContents file
    putStrLn contents
    hClose file
```

Dans cet exemple, nous ouvrons le fichier "monfichier.txt" en mode lecture, stockons son contenu dans la variable "contents" et l'affichons en utilisant la fonction "putStrLn". Enfin, nous fermons le fichier en utilisant la fonction "hClose". L'avantage de cette approche est que le fichier est automatiquement fermé à la fin de l'exécution du programme, même en cas d'erreurs.

Vous pouvez également utiliser la fonction "withFile" pour ouvrir et manipuler le fichier en une seule étape. La syntaxe est la suivante :

```Haskell
import System.IO
 
main = do
    withFile "monfichier.txt" ReadMode (\handle -> do
        contents <- hGetContents handle
        putStrLn contents)
```

## Plongée en profondeur

En utilisant la fonction "hGetContents", l'intégralité du contenu du fichier est chargée en mémoire sous la forme d'une chaine de caractères (String). Cela peut poser un problème pour les fichiers de grande taille car cela peut saturer la mémoire. Pour éviter cela, vous pouvez utiliser la fonction "hGetLine" pour lire le fichier ligne par ligne ou la fonction "hGetChar" pour lire le fichier caractère par caractère.

De plus, vous pouvez spécifier une limite de taille en utilisant la fonction "hGet" et en spécifiant le nombre de caractères à lire. Enfin, si vous n'avez pas spécifié de mode d'ouverture de fichier, Haskell utilisera automatiquement le mode lecture par défaut.

## Voir aussi

- [Documentation officielle de Haskell](https://www.haskell.org/documentation/)
- [Tutoriel sur la programmation Haskell](https://www.tutorialspoint.com/haskell/index.htm)
- [Liste de modules utiles pour la manipulation de fichiers en Haskell](https://hackage.haskell.org/packages/search?terms=file)