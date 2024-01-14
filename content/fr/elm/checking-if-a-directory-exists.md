---
title:    "Elm: Vérification de l'existence d'un répertoire"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/elm/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Pourquoi

Saviez-vous qu'il est possible en Elm de vérifier si un répertoire existe ? Si vous êtes développeur web et que vous travaillez avec des fichiers et des dossiers, cette fonctionnalité peut être très utile. Dans cet article, nous allons explorer pourquoi et comment vous pouvez vérifier l'existence d'un répertoire en Elm.

## Comment faire

Pour vérifier si un répertoire existe en Elm, nous pouvons utiliser la fonction `Directory.exists` qui prend en paramètre le chemin du répertoire que nous voulons vérifier. Le chemin peut être absolu ou relatif. Voici un exemple de code:

```elm
Directory.exists "mon/dossier/monFichier.txt"

-- output : True ou False
```

Comme vous pouvez le voir, la fonction renvoie un `True` si le répertoire existe et un `False` s'il n'existe pas. Il est également possible d'utiliser cette fonction avec la commande `Remote.FileSystem` pour vérifier l'existence de répertoires sur un serveur distant.

## Plongée en profondeur

Maintenant que nous savons comment utiliser la fonction `Directory.exists`, examinons plus en détail comment elle fonctionne. Cette fonction utilise un système de permissions pour déterminer si un répertoire existe ou non. Si vous n'avez pas la permission d'accéder à un répertoire, alors la fonction renverra `False`, même si le répertoire existe réellement.

Il est également important de noter que cette fonction ne peut vérifier que l'existence d'un répertoire, pas son type (fichier ou dossier). Si vous avez besoin de vérifier si un élément est un répertoire ou un fichier, vous devrez utiliser la fonction `Directory.isDir` ou `Directory.isFile` respectivement.

## Voir aussi

- Le site officiel d'Elm : https://elm-lang.org/
- La documentation sur la fonction `Directory.exists` : https://package.elm-lang.org/packages/elm/file/latest/Directory#exists
- Un exemple d'utilisation de la fonction `Directory.exists` : https://gist.github.com/eimarantobe/e6490f5144b7ca6091643c62e4fa5f56