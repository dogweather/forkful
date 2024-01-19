---
title:                "Lecture des arguments de ligne de commande"
html_title:           "Ruby: Lecture des arguments de ligne de commande"
simple_title:         "Lecture des arguments de ligne de commande"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi?

La lecture des arguments de ligne de commande est une pratique que les programmeurs utilisent pour obtenir des informations de l'utilisateur. Cela permet à l'utilisateur de diriger le comportement du programme en entrant des valeurs spécifiques lors de l'exécution.

## Comment faire :

Voici un exemple simple. Utilisons le module `System.Environment`.

```Haskell
import System.Environment

main = do
    args <- getArgs
    print args
```

Quand vous exécutez un programme avec des arguments comme `./program arg1 arg2`, `args` devient une liste de chaînes de caractères : `["arg1","arg2"]`.

## Plongée en profondeur

Historiquement, la lecture des arguments de ligne de commande est une pratique commune depuis l'aube de l'informatique, où les interfaces de ligne de commande étaient la norme.

Pour affiner votre contrôle, vous pouvez utiliser la fonction `getProgName` du même module, qui retourne le nom du programme.

```Haskell
name <- getProgName 
```

Dans Haskell, il existe aussi des bibliothèques plus sophistiquées pour la gestion des arguments de ligne de commande, comme `optparse-applicative` et `cmdargs`.

## Voir aussi

Pour plus d'information, consultez les documents officiels sur `System.Environment`: http://hackage.haskell.org/package/base-4.12.0.0/docs/System-Environment.html 

Pour un guide sur `optparse-applicative`, voir ici: https://github.com/pcapriotti/optparse-applicative 

Pour `cmdargs`, consultez: https://github.com/ndmitchell/cmdargs