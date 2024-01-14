---
title:                "Haskell: Vérification de l'existence d'un répertoire"
simple_title:         "Vérification de l'existence d'un répertoire"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Pourquoi

L'une des tâches les plus courantes en programmation est de vérifier si un répertoire existe. Cela peut être utile pour éviter les erreurs lors de l'accès à des fichiers ou pour s'assurer qu'un programme fonctionne correctement en s'assurant que les éléments dont il a besoin sont bien présents.

## Comment faire

Pour vérifier si un répertoire existe en Haskell, nous pouvons utiliser la fonction `doesDirectoryExist` du module `System.Directory`. Cette fonction prend en paramètre le chemin du répertoire à vérifier et renvoie un booléen indiquant si le répertoire existe ou non.

```Haskell
import System.Directory

main = do
  exists <- doesDirectoryExist "chemin/vers/repertoire"
  if exists
    then putStrLn "Le répertoire existe."
    else putStrLn "Le répertoire n'existe pas."
```

Si le répertoire existe, la sortie sera "Le répertoire existe." Sinon, elle sera "Le répertoire n'existe pas.".

## Profondeur de plongée

Il est important de noter que la fonction `doesDirectoryExist` ne vérifie pas si le chemin donné mène à un répertoire ou à un fichier. Elle vérifie simplement si un élément du système de fichiers existe avec le nom donné. Pour vérifier s'il s'agit bien d'un répertoire, nous pouvons utiliser la fonction `doesPathExist` du même module, qui fonctionne de la même manière mais renvoie un booléen indiquant si le chemin mène à un répertoire ou non.

De plus, il existe d'autres fonctions utiles dans le module `System.Directory` pour travailler avec des répertoires, telles que `createDirectory` pour créer un répertoire et `removeDirectory` pour le supprimer.

## Voir aussi

- [Documentation du module `System.Directory`](https://hackage.haskell.org/package/directory/docs/System-Directory.html)
- [Tutoriel sur la manipulation des répertoires en Haskell](https://wiki.haskell.org/Working_with_files)
- [Exemples pratiques de vérification des répertoires en Haskell](https://hackage.haskell.org/package/missingh-1.4.1/docs/System-IO-Utils.html#g:15)

À bientôt pour un nouveau tutoriel Haskell !