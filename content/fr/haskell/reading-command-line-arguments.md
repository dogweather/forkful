---
title:    "Haskell: Lecture des arguments de ligne de commande"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un programmeur en herbe ou expérimenté, il est toujours important de continuer à apprendre de nouvelles compétences et de nouvelles langues de programmation. L'une de ces compétences essentielles est la capacité de lire les arguments de ligne de commande dans vos programmes. Cela peut sembler intimidant au début, mais cela peut grandement améliorer la qualité et la flexibilité de vos programmes.

## Comment faire

Pour lire les arguments de ligne de commande en Haskell, vous pouvez utiliser la fonction `getArgs` du module `System.Environment`. Voici un exemple de code qui lit un argument de ligne de commande et l'affiche à l'écran :

```Haskell
import System.Environment

main = do
  args <- getArgs
  putStrLn ("L'argument de ligne de commande est : " ++ head args)
```

Si vous exécutez ce programme avec l'argument "123", vous devriez obtenir la sortie suivante :

```bash
$ runhaskell readArgs.hs 123
L'argument de ligne de commande est : 123
```

Comme vous pouvez le voir, la valeur de l'argument de ligne de commande a été stockée dans la variable `args` sous la forme d'une liste de chaînes de caractères. Vous pouvez ensuite utiliser cette liste pour effectuer des opérations supplémentaires dans votre programme.

## Plongée en profondeur

Maintenant que vous savez comment lire les arguments de ligne de commande en Haskell, il est important de comprendre comment ils sont traités en interne. Lorsque vous exécutez un programme Haskell avec des arguments de ligne de commande, ceux-ci sont stockés dans le tableau `argv` de la fonction `main`, qui peut ensuite être manipulé à l'aide de la fonction `getArgs`.

De plus, il est également possible de spécifier des options avec les arguments de ligne de commande en utilisant le package `optparse-applicative`, qui offre une syntaxe claire et concise pour gérer les options, les arguments obligatoires et facultatifs, ainsi que l'affichage de l'aide.

## Voir aussi

Pour plus d'informations sur la lecture des arguments de ligne de commande en Haskell, voici quelques liens utiles :

- [Documentation officielle de Haskell pour le module `System.Environment`](https://hackage.haskell.org/package/base-4.15.0.0/docs/System-Environment.html)
- [Guide pratique pour lire les arguments de ligne de commande en Haskell](https://wiki.haskell.org/Command_line_option_parsing/en)
- [Packag `optparse-applicative` pour gérer les options en ligne de commande en Haskell](https://hackage.haskell.org/package/optparse-applicative)

Maintenant que vous avez appris les bases pour lire les arguments de ligne de commande en Haskell, n'hésitez pas à explorer ces liens pour en savoir plus et améliorer vos compétences en programmation fonctionnelle. Bonne chance !