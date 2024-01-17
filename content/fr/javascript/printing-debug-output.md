---
title:                "Affichage du débogage"
html_title:           "Javascript: Affichage du débogage"
simple_title:         "Affichage du débogage"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/printing-debug-output.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi le faire?

La sortie de débogage est un moyen pour les programmeurs de voir des informations supplémentaires sur leur code pendant le processus de développement. Il est généralement utilisé pour trouver et corriger les erreurs ou pour mieux comprendre le fonctionnement d'une partie spécifique du code.

## Comment faire:

Pour imprimer une sortie de débogage en Javascript, vous pouvez utiliser la méthode ```console.log()``` qui accepte un ou plusieurs arguments et les imprime dans la console du navigateur. Par exemple:

```Javascript
var name = "Jean";
var age = 25;
console.log("Bonjour, je m'appelle " + name + " et j'ai " + age + " ans.");
```
La sortie de débogage affichera: "Bonjour, je m'appelle Jean et j'ai 25 ans."

## Plongée en profondeur:

La sortie de débogage est apparue dès les premiers jours de la programmation informatique pour aider les programmeurs à trouver des erreurs dans leur code. Mais au fil du temps, elle est devenue un outil utile pour comprendre le code et pour le développer de manière plus efficace. Bien que ```console.log()``` soit la méthode la plus couramment utilisée pour imprimer une sortie de débogage en Javascript, il existe également d'autres méthodes telles que ```console.error()``` pour afficher des messages d'erreur ou ```console.warn()``` pour afficher des avertissements.

## Voir aussi:

Pour en savoir plus sur la sortie de débogage en Javascript, vous pouvez consulter ces sources:
- [Documentation MDN de console.log](https://developer.mozilla.org/fr/docs/Web/API/Console/log)
- [Vidéo YouTube sur l'utilisation de la sortie de débogage en Javascript](https://www.youtube.com/watch?v=-sblJk2Nq-o)
- [Article Medium sur l'importance de la sortie de débogage](https://medium.com/the-code-smith/how-can-console-log-save-your-life-afc2b0cfb0ff)