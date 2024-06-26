---
date: 2024-01-20 17:56:11.514412-07:00
description: 'How to: Haskell rend la lecture des arguments de la ligne de commande
  assez simple avec le module `System.Environment`.'
lastmod: '2024-03-13T22:44:57.849934-06:00'
model: gpt-4-1106-preview
summary: Haskell rend la lecture des arguments de la ligne de commande assez simple
  avec le module `System.Environment`.
title: Lecture des arguments de ligne de commande
weight: 23
---

## How to:
Haskell rend la lecture des arguments de la ligne de commande assez simple avec le module `System.Environment`.

```haskell
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    case args of
        [name, age] -> putStrLn $ "Bonjour, " ++ name ++ "! Vous avez " ++ age ++ " ans."
        _           -> putStrLn "Usage: ./your_program <name> <age>"
```

Si vous compilez ce code et que vous l'exécutez avec des arguments, voici ce que cela donne :

```bash
$ ./your_program Jean 30
Bonjour, Jean! Vous avez 30 ans.
```

## Deep Dive
La fonction `getArgs` fait partie de `System.Environment` et existe depuis les premières versions de Haskell. C'est la méthode de base pour accéder aux arguments de la ligne de commande, mais il existe des bibliothèques plus sophistiquées, comme `optparse-applicative`, qui offrent un parsing plus élaboré et des messages d'aide automatiques.

Sous le capot, `getArgs` appelle le code de votre système d'exploitation pour obtenir les arguments. Haskell repose sur une abstraction IO monad qui rend les effets secondaires comme la lecture des arguments explicitement gérés dans les types.

Par rapport aux autres langages, Haskell est unique avec ses fonctions pures et l'IO monad. Cette séparation renforce la fiabilité du code, mais peut être inhabituelle pour les nouveaux venus.

## See Also
- Documentation de `System.Environment`: http://hackage.haskell.org/package/base-4.15.0.0/docs/System-Environment.html
- `optparse-applicative` pour des options de ligne de commande avancées : https://hackage.haskell.org/package/optparse-applicative
- Tutoriel sur l'IO monad pour une compréhension approfondie de la gestion des effets en Haskell : https://wiki.haskell.org/IO_inside
