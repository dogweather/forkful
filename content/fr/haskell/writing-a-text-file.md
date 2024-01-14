---
title:                "Haskell: Écrire un fichier texte."
programming_language: "Haskell"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Pourquoi écrire un fichier texte en Haskell ?

Si vous êtes programmeur ou développeur, vous savez sûrement l'importance des fichiers texte dans le monde de la programmation. Ils sont souvent utilisés pour stocker des données, configurer des applications et bien plus encore. Dans cet article, nous allons explorer comment écrire un fichier texte en Haskell et pourquoi cela peut être utile.

## Comment écrire un fichier texte en Haskell ?

Ecrire un fichier texte en Haskell est un processus simple en utilisant la fonction de base `writeFile`. Voici un exemple de code pour écrire un fichier texte contenant une liste de nombres :

```Haskell
writeFile "numbers.txt" $ unlines ["1", "2", "3", "4", "5"]
```

Ici, nous utilisons la fonction `unlines` pour transformer notre liste de nombres en une chaîne de caractères séparés par des sauts de ligne, que nous passons ensuite à la fonction `writeFile`. Cela va créer un nouveau fichier texte appelé "numbers.txt" avec les nombres sur chaque ligne.

Pour lire le contenu d'un fichier texte, nous pouvons utiliser la fonction `readFile` qui renvoie une chaîne de caractères du contenu du fichier. Voici un exemple pour lire et afficher le contenu de notre fichier "numbers.txt":

```Haskell
main = do
  contents <- readFile "numbers.txt"
  putStrLn contents
```

L'utilisation du `do` block est nécessaire car la fonction `readFile` est de type `IO String`, ce qui signifie qu'elle effectue une action en lecture et renvoie une chaîne de caractères.

## Plongée en profondeur

En utilisant des fonctions de manipulation de fichiers comme `readFile` et `writeFile`, il est possible de créer des applications Haskell qui peuvent lire et écrire des fichiers texte en toute simplicité. De plus, vous pouvez utiliser des fonctions telles que `lines` ou `words` pour convertir une chaîne de caractères en liste de lignes ou de mots, respectivement.

Cependant, il est important de noter que les fichiers texte doivent être manipulés avec soin, car ils peuvent facilement être corrompus si les bonnes précautions ne sont pas prises. Il est également possible d'utiliser des librairies externes pour lire et écrire des fichiers plus complexes, tels que les fichiers CSV ou JSON.

## Voir aussi

Voici quelques ressources supplémentaires pour en apprendre davantage sur la manipulation de fichiers texte en Haskell :

- [Documentation officielle de Haskell sur les opérations de fichiers](https://wiki.haskell.org/Introduction#File_Manipulation)
- [Tutoriel vidéo sur la manipulation de fichiers en Haskell](https://www.youtube.com/watch?v=w-e8sS2VZHc)
- [Article sur la manipulation des fichiers CSV en Haskell](https://hackernoon.com/parsing-csv-data-in-haskell-a9490b3ce8be)

Maintenant que vous avez appris à écrire un fichier texte en Haskell, vous pouvez explorer d'autres options pour manipuler des fichiers et simplifier vos tâches de programmation. Amusez-vous bien !