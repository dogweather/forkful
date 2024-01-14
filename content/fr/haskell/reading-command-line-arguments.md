---
title:                "Haskell: Lecture des arguments de ligne de commande"
simple_title:         "Lecture des arguments de ligne de commande"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Pourquoi 

Il y a de nombreuses raisons pour lesquelles vous pourriez vouloir lire les arguments de ligne de commande en Haskell. Peut-être que vous devez créer un programme qui réagit en fonction des options sélectionnées par l'utilisateur. Ou peut-être que vous souhaitez simplement obtenir des informations supplémentaires à partir de la ligne de commande pour votre programme. Quelle que soit la raison, la lecture des arguments de ligne de commande peut être une compétence précieuse pour tout programmeur Haskell.

# Comment faire 

Voici un exemple de code Haskell pour lire des arguments de ligne de commande :

```
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    putStrLn ("Le premier argument est " ++ head args)
    putStrLn ("Le deuxième argument est " ++ args !! 1)
```

Voici un exemple d'entrée et de sortie pour mieux comprendre :

**Entrée :**
```
HaskellProg.exe foo bar
```

**Sortie :**
```
Le premier argument est foo
Le deuxième argument est bar
```

Dans cet exemple, nous utilisons la fonction `getArgs` du module `System.Environment` pour obtenir la liste des arguments de ligne de commande. Ensuite, nous utilisons la fonction `head` pour accéder au premier élément de la liste et la fonction `!!` pour accéder au deuxième élément.

Vous pouvez également utiliser une boucle pour parcourir tous les arguments :

```
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    mapM_ putStrLn args
```

Dans cet exemple, nous utilisons la fonction `mapM_` pour appliquer la fonction `putStrLn` à chaque élément de la liste des arguments.

# Plongée en profondeur

Si vous souhaitez en savoir plus sur la lecture des arguments de ligne de commande en Haskell, voici quelques points supplémentaires à prendre en compte :

- Vous pouvez également utiliser la fonction `getProgName` pour obtenir le nom du programme à partir des arguments de ligne de commande.
- Si votre programme prend des options, vous voudrez peut-être utiliser un package comme `optparse-applicative` pour les gérer plus facilement.
- Assurez-vous de gérer les erreurs lorsque la liste d'arguments est vide ou lorsque vous essayez d'accéder à un élément qui n'existe pas.

# Voir aussi

- [Documentation officielle sur les arguments de ligne de commande en Haskell](https://www.haskell.org/documentation/#getting-started)
- [Module System.Environment](https://hackage.haskell.org/package/base-4.15.0.0/docs/System-Environment.html)
- [Package optparse-applicative](https://hackage.haskell.org/package/optparse-applicative)