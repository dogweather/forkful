---
title:                "Écriture vers l'erreur standard"
html_title:           "Javascript: Écriture vers l'erreur standard"
simple_title:         "Écriture vers l'erreur standard"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Pourquoi 

Écrire vers l'erreur standard (standard error) est une pratique courante dans le développement Javascript. Cela permet de repérer les erreurs et les bugs plus facilement lors de l'exécution du code, ce qui permet un débogage efficace.

## Comment faire 

Pour écrire vers l'erreur standard en JavaScript, il suffit d'utiliser la fonction `console.error()` en y passant le message à afficher comme paramètre. Exemple : 

```Javascript
console.error("Erreur : variable non définie");
```

Cela affichera le message "Erreur : variable non définie" dans la console d'erreur. 

## Plongée en profondeur 

Souvent confondue avec la console de log (console log), la console d'erreur (console error) est spécifiquement utilisée pour afficher des messages d'erreur. Elle est utile pour identifier rapidement les problèmes dans le code pendant le processus de développement ou même en production. De plus, la console d'erreur peut également afficher les informations de la pile d'appels (stack trace) pour aider à localiser l'origine de l'erreur.

## Voir aussi 

- [Documentation MDN sur la console.error()](https://developer.mozilla.org/fr/docs/Web/API/Console/error)
- [Article sur l'utilisation de la console dans les navigateurs web](https://www.alsacreations.com/tuto/lire/582-La-console-dans-les-navigateurs-web.html)
- [Guide du débogage en JavaScript](https://www.w3schools.com/js/js_debugging.asp)