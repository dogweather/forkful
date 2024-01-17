---
title:                "Afficher la sortie de débogage"
html_title:           "Gleam: Afficher la sortie de débogage"
simple_title:         "Afficher la sortie de débogage"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/printing-debug-output.md"
---

{{< edit_this_page >}}

#Qu'est-ce que c'est et pourquoi le faire?

Le "debug output" c'est simplement le fait d'afficher des informations pendant que votre code s'exécute pour le déboguer ou le comprendre. Les programmeurs le font pour trouver des erreurs, suivre le déroulement de leur code et mieux comprendre comment il fonctionne.

#Comment faire:

Pour afficher du "debug output" dans Gleam, utilisez la fonction `debug` et placez l'information que vous voulez afficher comme argument. Voici un exemple:

```
Gleam.debug("Voici un message de débogage")
```

Cela affichera "Voici un message de débogage" dans la console lorsque le code sera exécuté.

#Plongée en profondeur:

Afficher des informations de débogage n'est pas une pratique nouvelle. Les programmeurs l'utilisent depuis longtemps pour aider à comprendre et dépanner leur code. Il existe également d'autres moyens de déboguer, tels que l'utilisation de débogueurs spéciaux, mais afficher du "debug output" reste une méthode simple et efficace.

Dans Gleam, la fonction `debug` est implémentée à l'aide d'un module standard appelé `gleam_io`. Ce module fournit des fonctions pour l'entrée et la sortie de données, y compris l'affichage du "debug output".

#Voir aussi:

Pour plus d'informations sur le débogage en général, consultez ces liens utiles:

- [Site officiel de Gleam](https://gleam.run/documentation/) pour plus d'informations sur la fonction `debug` et d'autres fonctionnalités utiles de Gleam.
- [Article Wikipédia sur le débogage](https://fr.wikipedia.org/wiki/D%C3%A9bogage) pour en apprendre davantage sur l'historique et les différentes méthodes de débogage.
- [Vidéo YouTube sur le débogage en pratique](https://www.youtube.com/watch?v=a2Q8AETVpwE) pour voir comment les programmeurs utilisent le "debug output" dans leur processus de développement.