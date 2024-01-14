---
title:    "TypeScript: Imprimer la sortie de débogage"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/printing-debug-output.md"
---

{{< edit_this_page >}}

## Pourquoi

L'utilisation de la sortie de débogage dans la programmation est essentielle pour comprendre le fonctionnement de votre code. Cela permet de repérer les erreurs et de comprendre comment votre programme s'exécute. Sans la sortie de débogage, il peut être difficile de résoudre les problèmes et d'améliorer votre code.

## Comment faire

Pour imprimer une sortie de débogage en TypeScript, vous pouvez utiliser la fonction console.log(). Cette fonction prend un ou plusieurs paramètres à imprimer dans la console. Voici un exemple:

```TypeScript
console.log("Bonjour!");
```

Ce code imprime simplement "Bonjour!" dans la console lors de l'exécution du programme.

Vous pouvez également utiliser la fonction console.debug() pour afficher des informations de débogage supplémentaires. Ces messages seront utiles pour comprendre le flux d'exécution de votre programme et repérer les éventuelles erreurs.

```TypeScript
let num = 10;
console.debug("La valeur de num est", num);
```

Lors de l'exécution, ce code affichera "La valeur de num est 10" dans la console.

## Plongée en profondeur

Il y a plusieurs façons d'améliorer l'utilisation de la sortie de débogage dans votre code TypeScript. Vous pouvez, par exemple, utiliser des niveaux de débogage pour filtrer les messages que vous souhaitez afficher dans la console. Cela peut être utile lorsque votre code devient plus complexe et que vous avez besoin de cibler des parties spécifiques de votre programme pour le débogage.

De plus, vous pouvez utiliser des bibliothèques tierces telles que "debug" pour plus de fonctionnalités de débogage avancées. De telles bibliothèques peuvent vous aider à organiser vos messages de débogage de manière plus structurée et à les désactiver facilement en cas de besoin.

## Voir aussi

- [Documentation officielle de TypeScript sur la fonction console](https://www.typescriptlang.org/docs/handbook/console.html)
- [Utilisation du débogage en TypeScript](https://khalilstemmler.com/articles/typescript-tutorial/debugging-typescript-in-vscode/)
- [Utilisation de la bibliothèque de débogage "debug"](https://www.npmjs.com/package/debug)