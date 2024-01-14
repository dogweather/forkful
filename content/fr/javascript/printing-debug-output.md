---
title:                "Javascript: Affichage du débogage"
programming_language: "Javascript"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/printing-debug-output.md"
---

{{< edit_this_page >}}

## Pourquoi

Lorsque vous êtes en train de programmer en Javascript, vous pouvez rencontrer des problèmes avec votre code. Peut-être que votre programme ne s'exécute pas comme prévu ou qu'il produit une erreur. Dans ces situations, il peut être très utile d'utiliser l'impression de sortie de débogage pour comprendre ce qui se passe dans votre code.

## Comment Faire

L'impression de sortie de débogage en Javascript peut se faire de plusieurs manières. La plus simple consiste à utiliser la méthode `console.log()`. Vous pouvez l'utiliser pour imprimer des valeurs, des variables ou même des messages dans la console de votre navigateur. Par exemple :

```Javascript
let num = 10;
console.log("Le nombre est " + num);
```

Cela imprimera dans la console : `Le nombre est 10`. Vous pouvez également utiliser `console.error()` pour afficher des messages d'erreur ou `console.warn()` pour des avertissements.

Il est également possible d'utiliser des outils de débogage intégrés dans les navigateurs, comme l'onglet "Console" dans l'inspecteur de Google Chrome ou l'onglet "Console" dans les outils de développement de Mozilla Firefox. Ces outils offrent des fonctionnalités avancées telles que l'affichage des étapes d'exécution de votre code et la possibilité d'ajouter des points d'arrêt pour arrêter l'exécution à un certain endroit.

## Plongée Profonde

L'impression de sortie de débogage peut être une technique très utile lorsque vous rencontrez des problèmes avec votre code. Elle peut vous aider à comprendre quelles parties de votre code sont exécutées et quelles valeurs ont vos variables à un certain moment. Cependant, il est important de ne pas laisser de messages de débogage dans votre code final, car cela peut affecter les performances et rendre votre code plus difficile à lire.

De plus, il existe des techniques plus avancées pour le débogage en Javascript, telles que l'utilisation de "breakpoints" dans votre code, l'utilisation du débogueur dans votre éditeur de code et l'utilisation de bibliothèques de débogage telles que `debug.js` ou `bugsnag.js`.

En fin de compte, il est important de se rappeler que l'impression de sortie de débogage ne doit être utilisée que pendant le processus de développement et qu'elle doit être supprimée avant d'envoyer votre code en production.

## Voir Aussi

Pour en savoir plus sur le débogage en Javascript, vous pouvez consulter les ressources suivantes :

- [Guide de Débogage de Google Chrome](https://developers.google.com/web/tools/chrome-devtools/javascript)
- [Guide de Débogage de Firefox](https://developer.mozilla.org/fr/docs/Tools/Debugger)
- [Documentation de `debug.js`](https://github.com/visionmedia/debug)
- [Documentation de `bugsnag.js`](https://docs.bugsnag.com/platforms/javascript/)