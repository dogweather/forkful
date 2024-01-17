---
title:                "Lecture des arguments en ligne de commande"
html_title:           "Haskell: Lecture des arguments en ligne de commande"
simple_title:         "Lecture des arguments en ligne de commande"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

Que sont les arguments de ligne de commande et pourquoi les programmeurs les utilisent-ils?

Les arguments de ligne de commande sont des informations supplémentaires fournies à un programme lors de son exécution. Ils sont généralement utilisés pour personnaliser le comportement d'un programme et peuvent être saisis par l'utilisateur au moment de l'exécution. Les programmeurs utilisent des arguments de ligne de commande pour rendre leurs programmes plus flexibles et personnalisables pour les utilisateurs.

Comment faire:

Pour lire des arguments de ligne de commande en Haskell, vous pouvez utiliser la fonction `getArgs` du module `System.Environment`. Cette fonction renvoie une liste de chaînes représentant les arguments saisis par l'utilisateur. Voici un exemple de code montrant comment utiliser cette fonction:

```Haskell
import System.Environment

main = do
  args <- getArgs
  putStrLn ("Le premier argument est: " ++ head args)
```

Lorsque vous exécutez ce programme avec l'argument `bonjour`, vous devriez voir la sortie suivante:

```
Le premier argument est: bonjour
```

Plongée en profondeur:

La lecture des arguments de ligne de commande est une fonctionnalité courante dans de nombreux langages de programmation. Elle est particulièrement utile pour les programmes en ligne de commande, mais peut également être utilisée dans des applications plus complexes.

Il existe plusieurs alternatives à la fonction `getArgs` en Haskell, telles que `HaskellArgs` et `ArgsParser`. Chacune offre des fonctionnalités supplémentaires pour gérer les arguments de ligne de commande de manière plus flexible.

L'implémentation de la fonction `getArgs` utilise le paquet `System.Environment` qui fait partie de la bibliothèque standard de Haskell. Ce paquet fournit également d'autres fonctions utiles pour gérer l'environnement d'exécution du programme.

Voir aussi:

- [HaskellArgs](https://hackage.haskell.org/package/HaskellArgs)
- [ArgsParser](https://hackage.haskell.org/package/ArgsParser)
- [Module System.Environment](https://hackage.haskell.org/package/base-4.15.0.0/docs/System-Environment.html)