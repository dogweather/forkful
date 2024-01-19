---
title:                "Imprimer la sortie de débogage"
html_title:           "Arduino: Imprimer la sortie de débogage"
simple_title:         "Imprimer la sortie de débogage"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/printing-debug-output.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
L'impression de debug est une méthode utilisée par les programmeurs pour suivre l'exécution de leur code. C'est une technique précieuse pour détecter et résoudre les bugs dans le code, voir l'état des variables à un instant précis et comprendre le flux du programme.

## Comment faire :
En TypeScript, on utilise surtout `console.log()` pour imprimer le debug. Voici quelques exemples de base:

```TypeScript
let nom: string = "Pierre";
console.log(nom); // Sortie: Pierre

let age: number = 35;
console.log(`J'ai ${age} ans.`); // Sortie: J'ai 35 ans.
```

Vous pouvez aussi imprimer les objets et les tableaux comme ceci :

```TypeScript
let tableau: number[] = [1, 2, 3];
console.log(tableau); // Sortie : [1, 2, 3]

let objet: { [key: string]: any; } = { "nom": "Marie", "age": 28 };
console.log(objet); // Sortie : { "nom": "Marie", "age": 28 }
```

## Approfondissement
Historiquement, le debug a été introduit par les programmeurs d'assemblage pour suivre leur code linéaire. Avec l'évolution des langages de programmation, les méthodes de debug sont devenues plus sophistiquées.

Il existe d'autre méthodes pour le debug en TypeScript comme utiliser un débogueur TypeScript intégré dans un IDE comme Visual Studio Code, WebStorm etc. Vous pouvez également utiliser des outils externes pour le debug de code Node.js comme `node-inspect` ou `iron-node`.

La méthode `console.log()` est une partie de l'interface de console Web API présente dans le JavaScript. Elle est supportée par tous les navigateurs modernes et a été adaptée pour Node.js. C'est pourquoi il est recommandé d'utiliser cette méthode pour le debug du code TypeScript.

## Voir Aussi
Pour plus d'informations et des exemples de code sur le debug en TypeScript, consultez ces ressources :

1. [Documentation de TypeScript](https://www.typescriptlang.org/docs/)
2. [Debugging TypeScript in VS Code](https://code.visualstudio.com/docs/typescript/typescript-debugging)
3. [Debugging Node.js with TypeScript in VS Code](https://medium.com/@david.garcia_42524/debugging-typescript-in-vs-code-94e8b4fd769)