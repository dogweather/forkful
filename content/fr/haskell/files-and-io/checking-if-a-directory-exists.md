---
title:                "Vérifier si un répertoire existe"
aliases:
- /fr/haskell/checking-if-a-directory-exists/
date:                  2024-02-03T19:07:24.209008-07:00
model:                 gpt-4-0125-preview
simple_title:         "Vérifier si un répertoire existe"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/checking-if-a-directory-exists.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Vérifier si un répertoire existe est une opération fondamentale dans de nombreuses tâches de programmation, permettant des actions conditionnelles basées sur la présence ou l'absence de structures de répertoires. C'est crucial pour la manipulation de fichiers, les scripts automatisés, et lors de la configuration initiale du logiciel pour s'assurer que les répertoires nécessaires sont en place, ou pour éviter de dupliquer les répertoires.

## Comment faire :
Haskell, grâce à sa bibliothèque de base, offre des moyens simples de vérifier l'existence d'un répertoire, principalement en utilisant le module `System.Directory`. Voyons un exemple de base :

```haskell
import System.Directory (doesDirectoryExist)

main :: IO ()
main = do
  let dirPath = "/chemin/vers/votre/repertoire"
  exists <- doesDirectoryExist dirPath
  putStrLn $ "Le répertoire existe-t-il ? " ++ show exists
```

Exemple de sortie, selon que le répertoire existe ou non :

```
Le répertoire existe-t-il ? True
```
Ou :
```
Le répertoire existe-t-il ? False
```

Pour des scénarios plus complexes ou une fonctionnalité supplémentaire, vous pourriez envisager une bibliothèque tierce populaire comme `filepath` pour manipuler et gérer les chemins de fichiers de manière plus abstraite. Cependant, pour le but simple de vérifier si un répertoire existe, le `System.Directory` de la bibliothèque de base est suffisant et efficace.

Rappelez-vous, travailler avec des systèmes de fichiers peut varier selon les plateformes, et l'approche de Haskell vise à abstraire certaines de ces différences. Testez toujours vos opérations de fichiers sur le système cible pour garantir un comportement attendu.
