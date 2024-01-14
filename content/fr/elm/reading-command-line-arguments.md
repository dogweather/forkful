---
title:    "Elm: Lecture des arguments de ligne de commande"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/elm/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un développeur Elm, vous savez probablement que ce langage est connu pour sa forte typisation et sa sûreté de l'information. Cependant, saviez-vous qu'il est également possible de lire des arguments de ligne de commande en utilisant Elm ? Dans cet article, nous allons explorer pourquoi il peut être utile de lire les arguments de ligne de commande et comment le faire en utilisant Elm.

## Comment Faire

Pour lire les arguments de ligne de commande en Elm, nous allons utiliser la fonction `PlatformWorker.getCommandLineArgs`. Elle prend une fonction en argument qui sera appelée avec une liste d'arguments en chaîne de caractères. Voici un exemple de code pour imprimer la liste des arguments sur la console :

```Elm
import PlatformWorker

main =
  PlatformWorker.getCommandLineArgs (\args -> 
    --  Imprime la liste des arguments sur la console
    List.map Debug.log args
  )
```

Si vous exécutez ce code avec les arguments `elm make src/Main.elm --output=dist/index.html`, vous verrez la sortie suivante sur la console :

`["src/Main.elm", "--output=dist/index.html"]`

Vous pouvez ensuite utiliser ces arguments dans votre programme comme vous le souhaitez.

## Plongée en Profondeur

Maintenant, vous vous demandez peut-être comment cette fonction `getCommandLineArgs` fonctionne réellement. En réalité, elle utilise une API native qui n'est pas écrite en Elm. En fait, cela est possible grâce à un paquetage spécial appelé `platform-worker`. Ce paquetage utilise l'API native disponible sur chaque plateforme pour communiquer avec le système d'exploitation et récupérer les arguments de ligne de commande.

Il est important de noter que cette fonctionnalité n'est pas disponible dans les navigateurs car les arguments de ligne de commande ne sont pas pertinents pour eux. Cependant, cela peut être très utile pour les applications de bureau écrites en Elm.

## Voir Aussi

Pour plus d'informations sur la lecture des arguments de ligne de commande en Elm, voici quelques liens utiles :

- [Documentation officielle sur `PlatformWorker`](https://package.elm-lang.org/packages/elm/browser/latest/Browser#platform-worker)
- [Article sur la communication entre Elm et le système d'exploitation](https://elmprogramming.com/elm-system-module-part1.html)
- [Paquetage `platform-worker` sur GitHub](https://github.com/elm/core/tree/1.0.0)