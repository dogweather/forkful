---
title:                "Vérifier si un répertoire existe"
date:                  2024-01-20T14:56:31.184716-07:00
html_title:           "Go: Vérifier si un répertoire existe"
simple_title:         "Vérifier si un répertoire existe"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why? (Quoi & Pourquoi ?)
Vérifier si un répertoire existe permet de savoir si on peut y accéder ou non. Les programmeurs le font pour éviter des erreurs quand ils lisent, écrivent ou modifient des fichiers.

## How to (Comment faire)
```Haskell
import System.Directory (doesDirectoryExist)

main :: IO ()
main = do
  let dirPath = "path/to/your/directory"
  exists <- doesDirectoryExist dirPath
  putStrLn $ "Le répertoire " ++ dirPath ++ (if exists then " existe." else " n'existe pas.")
```
##### Résultat possible :
```
Le répertoire path/to/your/directory existe.
```
Ou, si le répertoire n'existe pas :
```
Le répertoire path/to/your/directory n'existe pas.
```

## Deep Dive (Plongée en profondeur)
Historiquement, la gestion des fichiers est cruciale en programmation. En Haskell, la bibliothèque `System.Directory` fournit des outils pour interagir avec le système de fichiers. Avant `doesDirectoryExist`, on pouvait lancer une commande système et interpréter le résultat, ce qui était plus complexe et sujet à erreurs. Il y a aussi `doesFileExist` pour les fichiers. Ces fonctions encapsulent des appels système spécifiques à la plateforme, ce qui simplifie le code et le rend portable.

## See Also (Voir aussi)
- Documentation de `System.Directory`: [https://hackage.haskell.org/package/directory-1.3.6.1/docs/System-Directory.html](https://hackage.haskell.org/package/directory-1.3.6.1/docs/System-Directory.html)