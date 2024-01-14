---
title:                "Haskell: Vérifier si un répertoire existe"
programming_language: "Haskell"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Pourquoi

La vérification de l'existence d'un répertoire peut être une tâche importante dans la programmation Haskell. Elle peut vous aider à vous assurer que votre programme peut accéder aux fichiers nécessaires et à éviter les erreurs potentielles.

## Comment faire

Pour vérifier si un répertoire existe en Haskell, nous pouvons utiliser la fonction `doesDirectoryExist` du module `System.Directory`. Voyons un exemple de code:

```Haskell
import System.Directory

directoryPath <- getCurrentDirectory -- obtenir le chemin du répertoire actuel
directoryExists <-  doesDirectoryExist directoryPath -- vérifier l'existence du répertoire
print directoryExists -- afficher le résultat
```

Lorsque nous exécutons ce code, nous obtenons le résultat `True` si le répertoire existe ou `False` s'il n'existe pas. Nous pouvons également spécifier un chemin de répertoire différent à la place de `getCurrentDirectory` pour vérifier un répertoire spécifique.

## Plongée en profondeur

La fonction `doesDirectoryExist` utilise le système de fichiers sous-jacent pour vérifier l'existence d'un répertoire. Elle renvoie une valeur de type `IO Bool` qui doit être utilisée dans une monade `IO`. De plus, si le chemin spécifié est un lien symbolique, la fonction vérifiera si le lien lui-même existe et non le chemin vers le répertoire réel.

## Voir aussi

- [Documentation de la fonction `doesDirectoryExist`](https://hackage.haskell.org/package/directory-1.3.6.1/docs/System-Directory.html#v:doesDirectoryExist)
- [Tutoriel sur les opérations de fichiers en Haskell](https://wiki.haskell.org/Introduction_to_IO)
- [Exemples de gestion de fichiers en Haskell](https://gist.github.com/annacrombie/06a07f0fff3b470cb564)