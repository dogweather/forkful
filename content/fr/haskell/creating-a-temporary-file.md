---
title:                "Créer un fichier temporaire."
html_title:           "Haskell: Créer un fichier temporaire."
simple_title:         "Créer un fichier temporaire."
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi le faire?

Créer un fichier temporaire est une pratique courante pour les programmeurs. Il s'agit de créer un fichier vide qui existe uniquement pour une durée limitée ou pour une tâche spécifique. Les programmeurs le font pour éviter d'encombrer leur système de fichiers avec des fichiers inutiles ou pour stocker des données qui ne seront utilisées qu'une seule fois.

## Comment faire ?

```Haskell
import System.IO
import System.Directory
import System.FilePath

main = do
    tempDir <- getTemporaryDirectory
    (tempFile, tempHandle) <- openTempFile tempDir "example.txt"
    putStrLn $ "Fichier temporaire créé : " ++ tempFile
    hPutStrLn tempHandle "Contenu du fichier temporaire"
    hClose tempHandle
```

Output :

```
Fichier temporaire créé : /var/folders/f8/kjk123kalkjsdf/example.txt
```

## Plongée en profondeur

Créer des fichiers temporaires est une pratique courante en programmation depuis de nombreuses années. Avant les ordinateurs, les programmeurs le faisaient en utilisant du papier ou des cartes perforées pour stocker temporairement des données. De nos jours, il existe des alternatives telles que l'utilisation de la mémoire ou de bases de données temporaires pour stocker des données temporaires.

L'implémentation de la création d'un fichier temporaire dépend du langage de programmation utilisé. En Haskell, la fonction ```openTempFile``` utilise le répertoire de l'utilisateur par défaut pour stocker le fichier temporaire, mais il est également possible de spécifier un répertoire différent.

## À voir aussi

Pour en savoir plus sur la manipulation de fichiers en Haskell, vous pouvez consulter la documentation officielle : https://www.haskell.org/documentation/#files. Vous pouvez également trouver des exemples de création de fichiers temporaires dans des projets open source tels que Hackage : https://hackage.haskell.org/.