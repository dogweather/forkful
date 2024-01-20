---
title:                "Vérifier si un répertoire existe"
html_title:           "Haskell: Vérifier si un répertoire existe"
simple_title:         "Vérifier si un répertoire existe"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

La vérification de l'existence d'un répertoire est une opération simple mais cruciale en programmation. Il est important de vérifier si un répertoire existe avant d'essayer d'y écrire ou d'en lire des données afin d'éviter des erreurs inutiles.

## Comment faire :

En Haskell, nous utilisons la fonction `doesDirectoryExist` du module `System.Directory`. Voici un exemple simple :

```Haskell
import System.Directory

main = do
  putStrLn "Saisissez le chemin d'accès au répertoire :"
  dirPath <- getLine
  doesExist <- doesDirectoryExist dirPath
  if doesExist
    then putStrLn "Le répertoire existe."
    else putStrLn "Le répertoire n'existe pas."
```

Si le répertoire existe, le programme affiche "Le répertoire existe.". Si non, "Le répertoire n'existe pas.".

## Plongée Profonde :

Historiquement, la fonction `doesDirectoryExist` a été introduite dans le module `System.Directory` dans Glasgow Haskell Compiler (GHC) version 6.4.1, sortie en 2005. Par rapport à d'autres langages, Haskell fait du contrôle d'existence de répertoire une opération assez simple.

Il existe aussi l'alternative `doesPathExist` qui retourne `True` si le chemin spécifié mène à un répertoire **ou** à un fichier.

Sur le plan de l'implémentation, `doesDirectoryExist` utilise le système d'exploitation sous-jacent pour vérifier l'existence du répertoire, ce qui peut occasionner des différences minimes de comportement entre les plateformes.

## Voir Aussi :

1. Documentation Haskell pour `System.Directory`: https://hackage.haskell.org/package/directory-1.3.6.1/docs/System-Directory.html
2. Glasgow Haskell Compiler (GHC): https://www.haskell.org/ghc/
3. Un guide utile pour manipuler les fichiers et répertoires en Haskell : https://www.schoolofhaskell.com/school/starting-with-haskell/libraries-and-frameworks/text-io