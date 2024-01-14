---
title:    "Haskell: Lecture des arguments de ligne de commande"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Pourquoi

Lorsque vous écrivez un programme en Haskell, il est souvent nécessaire de prendre des entrées de l'utilisateur pour personnaliser le fonctionnement du programme. Cela peut inclure des chaînes de caractères, des nombres ou même des options spécifiques. La lecture des arguments de la ligne de commande est donc une compétence importante pour tout programmeur Haskell.

## Comment faire

Pour lire les arguments de la ligne de commande en Haskell, nous pouvons utiliser la fonction `getArgs` du module `System.Environment`. Cette fonction renvoie une liste de chaînes de caractères contenant tous les arguments passés au programme lors de son exécution. Par exemple :

```Haskell
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    putStrLn ("Les arguments sont : " ++ show args)
```

Supposons que le programme ci-dessus s'appelle "programme.hs" et qu'il soit exécuté avec les arguments "Hello" et "World" comme ceci : `runhaskell programme.hs Hello World`, il affichera la sortie suivante :

```
Les arguments sont : ["Hello", "World"]
```

## Plongée plus profonde

Bien que cela puisse sembler simple, la fonction `getArgs` peut en fait être problématique si les arguments contiennent des caractères spéciaux, tels que des guillemets ou des espaces. Dans ces cas, il est préférable d'utiliser la fonction `getArgsWith`, qui utilise un parser pour traiter correctement les arguments. Il est également important de noter que les arguments sont renvoyés sous forme de liste de chaînes de caractères, et qu'il est de la responsabilité du programmeur de les convertir en types appropriés si nécessaire.

## Voir aussi

- [Documentation sur `getArgs` et `getArgsWith`](http://hackage.haskell.org/package/base-4.14.1.0/docs/System-Environment.html#v:getArgs)
- [Tutoriel sur la manipulation des arguments de la ligne de commande en Haskell](https://www.tutorialspoint.com/haskell/haskell_command_line_arguments.htm)