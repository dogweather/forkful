---
title:                "Imprimer la sortie de débogage"
html_title:           "TypeScript: Imprimer la sortie de débogage"
simple_title:         "Imprimer la sortie de débogage"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/printing-debug-output.md"
---

{{< edit_this_page >}}

## Pourquoi

L'affichage du débogage est une pratique courante dans la programmation car elle peut grandement aider les développeurs à identifier et à résoudre les problèmes dans leur code. Avec TypeScript, il existe plusieurs façons d'afficher du débogage pour améliorer le processus de développement.

## Comment faire

Il existe deux principales méthodes pour afficher du débogage en TypeScript : utiliser la fonction console.log() ou utiliser le débogueur intégré dans votre environnement de développement.

```TypeScript
// Utilisation de console.log()
let name = "Pierre";
let age = 25;
console.log(`Mon nom est ${name} et j'ai ${age} ans.`);

// Résultat : Mon nom est Pierre et j'ai 25 ans.

//Utilisation du débogueur dans VS Code
function addition(a, b) {
  return a + b;
}

let result = addition(5, 7);
console.log(result);

// Placer un "point d'arrêt" sur la ligne du console.log()
// Exécutez le programme en mode de débogage
// Le débogueur s'arrêtera sur la ligne du point d'arrêt et vous pourrez inspecter les valeurs des variables
// Vous pouvez également parcourir votre code pas à pas pour comprendre son exécution et détecter les erreurs
```

Il est également possible d'utiliser des outils de débogage tiers spécialement conçus pour TypeScript, tels que le TypeScript Debugger for Chrome, qui permet de déboguer des applications TypeScript directement dans le navigateur Chrome.

## Plongée en profondeur

En plus de simplement afficher des valeurs de variables, le débogage en TypeScript peut également être utilisé pour suivre l'exécution de votre code et détecter des erreurs. Par exemple, en utilisant le débogueur intégré de VS Code, vous pouvez surveiller la valeur de vos variables pour détecter des changements suspects ou des boucles infinies.

De plus, le débogage peut être utile pour comprendre comment les différentes parties de votre code interagissent entre elles, en particulier dans les projets multi-fichiers. En utilisant des points d'arrêt stratégiquement placés, vous pouvez suivre l'exécution du code dans les différentes parties de votre application et ainsi mieux comprendre son fonctionnement.

## Voir aussi

Pour en savoir plus sur le débogage en TypeScript, vous pouvez consulter les ressources suivantes :

- [Documentation officielle de TypeScript pour le débogage](https://www.typescriptlang.org/docs/handbook/debugging.html)
- [Article de blog sur le débogage en TypeScript dans VS Code](https://code.visualstudio.com/docs/nodejs/nodejs-debugging)
- [TypeScript Debugger for Chrome](https://github.com/Microsoft/vscode-chrome-debug) - Une extension pour VS Code qui permet de déboguer du code TypeScript dans Chrome.