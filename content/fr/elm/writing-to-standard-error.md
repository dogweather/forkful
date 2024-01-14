---
title:    "Elm: Écrire sur l'erreur standard"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/elm/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Pourquoi

Écrire sur la sortie standard d'erreur est une pratique utile pour les développeurs qui veulent surveiller et déboguer leur code. En affichant des erreurs ou des messages de débogage précis, cela peut aider à identifier et résoudre rapidement les problèmes dans le code.

## Comment faire

Il existe plusieurs façons d'écrire sur la sortie standard d'erreur en Elm. La méthode la plus simple consiste à utiliser la fonction `Debug.log` qui prend en paramètre une chaîne de caractères et une valeur à afficher. Par exemple :

```elm
Debug.log "Mon message de débogage" variable
```

Cela affichera "Mon message de débogage: variable" sur la sortie standard d'erreur.

Pour afficher des messages d'erreur, vous pouvez utiliser la fonction `Debug.crash` qui prend en paramètre une chaîne de caractères représentant l'erreur. Par exemple :

```elm
Debug.crash "Le code a rencontré une erreur."
```

Cela affichera "Le code a rencontré une erreur." sur la sortie standard d'erreur et interrompra l'exécution du programme.

## Plongée en profondeur

L'utilisation de la fonction `Debug.log` peut également être utile pour vérifier l'état interne de votre programme. Vous pouvez y passer n'importe quelle variable pour voir sa valeur à un moment précis de l'exécution. Cela peut être particulièrement utile pour trouver des erreurs difficiles à détecter. Cependant, n'oubliez pas de retirer toutes les fonctions `Debug.log` avant de mettre votre code en production car elles peuvent ralentir considérablement les performances.

De plus, l'utilisation de la fonction `Debug.crash` est réservée aux messages d'erreur graves. Évitez son utilisation pour le débogage car cela peut interrompre le bon fonctionnement du programme.

## Voir aussi

- [Documentation officielle d'Elm sur le débogage](https://guide.elm-lang.org/debugging/)
- [Article de blog sur l'utilisation de `Debug.log` en Elm](https://thoughtbot.com/blog/using-debug-log-in-elm)
- [Tutoriel vidéo sur la gestion des erreurs en Elm](https://www.youtube.com/watch?v=zH9ZdUko6wI)