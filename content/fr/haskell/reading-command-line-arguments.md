---
title:                "Haskell: Lecture des arguments de ligne de commande"
programming_language: "Haskell"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Pourquoi

Les arguments de ligne de commande sont un aspect essentiel de la programmation en Haskell. Ils permettent de personnaliser les actions d'un programme lors de son exécution. En comprendre le fonctionnement est donc crucial pour tout développeur utilisant ce langage fonctionnel.

## Comment Faire

Pour lire les arguments de ligne de commande en Haskell, nous allons utiliser une fonction appelée `getArgs`, qui renvoie une liste de chaînes de caractères représentant les arguments entrés par l'utilisateur.

```Haskell
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    putStrLn ("Les arguments entrés sont: " ++ show args)
```

Si nous exécutons ce code avec la commande `runhaskell monProgramme.hs Bonjour tout le monde`, nous obtiendrons l'output suivant:

`Les arguments entrés sont: ["Bonjour","tout","le","monde"]`

Nous pouvons ensuite utiliser ces arguments pour effectuer différentes actions dans notre programme, en fonction de nos besoins.

## Plongée Profonde

Il est important de noter que les arguments de ligne de commande en Haskell sont des chaînes de caractères. Par conséquent, si nous voulons utiliser ces arguments comme des entiers ou d'autres types de données, nous devrons les convertir en utilisant des fonctions telles que `read` et `fromIntegral`.

Il est également possible de manipuler les arguments avant de les utiliser, en utilisant des fonctions de manipulation de listes telles que `map` ou `filter`.

Pour plus d'informations, consultez la documentation officielle de Haskell sur les arguments de ligne de commande.

## Voir Aussi

- [Documentation officielle de Haskell sur les arguments de ligne de commande](https://www.haskell.org/onlinereport/haskell2010/haskellch11.html)
- [Tutoriel vidéo sur les arguments de ligne de commande en Haskell (en anglais)](https://www.youtube.com/watch?v=lNWcrapb_JU)