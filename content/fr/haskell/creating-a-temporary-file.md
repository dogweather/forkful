---
title:                "Création d'un fichier temporaire"
html_title:           "Kotlin: Création d'un fichier temporaire"
simple_title:         "Création d'un fichier temporaire"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# Créez un fichier temporaire en Haskell !

## Quoi & Pourquoi ?
La création d'un fichier temporaire, c'est faire un fichier provisoire qui est utilisé pour stocker des données de manière transitoire pendant l'exécution du programme. Les programmeurs le font généralement pour gérer de grandes quantités de données qui ne sont pas nécessaires une fois le programme terminé.

## Comment faire :
Voici comment vous créeriez un fichier temporaire en Haskell.
```Haskell
import System.IO
import System.IO.Temp

main = withSystemTempFile "prefix" $ \tempPath tempHandle -> do
  hPutStr tempHandle "Ce texte est temporaire!"
  hFlush tempHandle
  contents <- readFile tempPath -- reads the file back in
  putStrLn $ "I wrote: " ++ contents
```
L'échantillon ci-dessus écrit une ligne de texte dans le fichier temporaire, puis lit et affiche le contenu. 

## Exploration profonde :
La fonction `withSystemTempFile`, utilisée ci-dessus, a été introduite dans la version 4.3.3.0 de la bibliothèque `base` de Haskell. 

Les alternatives à l'utilisation des fichiers temporaires incluent les pipes nommées, les mémoires tampons en mémoire dédiées (RAM), et les bases de données temporaires. Le choix dépend de la quantité de données à gérer et des performances requises.

Les détails de l'implémentation diffèrent selon les plateformes. Par exemple, `mkstemp` est utilisé sur Unix pour générer un nom de fichier unique dans le dossier temporaire, tandis que `GetTempPath` et `CreateFile` sont utilisés sur Windows.

## Voir aussi :
- Documentation GHC pour [System.IO.Temp](http://hackage.haskell.org/package/temporary-1.3/docs/System-IO-Temp.html)
- Discussion StackOverflow sur [fichiers temporaires en Haskell](https://stackoverflow.com/questions/6001627/generating-temporary-file-names-in-haskell)
- Article en depth sur [la gestion des fichiers temporaires](https://www.haskell.org/tutorial/io.html#tempfiles) dans le Tutorial Haskell 
- Source code de la module [System.IO.Temp](https://hackage.haskell.org/package/temporary-1.3/docs/src/System-IO-Temp.html) sur Hackage