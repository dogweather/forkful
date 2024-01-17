---
title:                "Affichage de la sortie de débogage"
html_title:           "Haskell: Affichage de la sortie de débogage"
simple_title:         "Affichage de la sortie de débogage"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/printing-debug-output.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi le faire?

L'impression de sortie de debug est une pratique courante chez les programmeurs pour afficher des informations supplémentaires sur le fonctionnement d'un programme. Cela peut être utile pour déboguer des erreurs ou suivre le flux d'exécution d'un programme.

## Comment faire:

Voici un exemple de code Haskell qui utilise la fonction ```print``` pour afficher du texte dans la console:

```Haskell
main = do
  print "Bonjour, monde!"
```

Le résultat de l'exécution de ce code sera l'affichage du texte "Bonjour, monde!" dans la console.

## Plongeon plus profond:

L'impression de sortie de debug est une pratique qui existe depuis les premiers jours de la programmation. Avant l'introduction des outils de débogage modernes, c'était le seul moyen pour les programmeurs de suivre l'exécution de leur code.

Bien qu'elle reste utile dans certaines situations, l'impression de sortie de debug peut également être un moyen inefficace et fastidieux de déboguer un code complexe. Heureusement, il existe maintenant des outils plus sophistiqués tels que les débogueurs graphiques, les enregistreurs de stack trace et les analyseurs de performance.

L'implémentation de l'impression de sortie de debug dans Haskell est assez simple, grâce à la fonction standard ```print``` et à l'utilisation de la monade IO pour afficher les données dans la console.

## Voir aussi:

- [Haskell Wiki page on debugging](https://wiki.haskell.org/Debugging)
- [Debugging Techniques - A Functional Programming Approach](https://www.cs.kent.ac.uk/people/staff/sjt/AfpB/)
- [Debugging and Profiling in Haskell](https://www.fpcomplete.com/blog/2014/08/haskell-debugging-techniques)