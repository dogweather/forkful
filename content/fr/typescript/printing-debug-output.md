---
title:                "TypeScript: Affichage de sortie de débogage"
programming_language: "TypeScript"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/printing-debug-output.md"
---

{{< edit_this_page >}}

## Pourquoi

Imaginons que vous êtes en train de coder une application complexe en TypeScript et que vous rencontrez des problèmes. Vous avez probablement utilisé des techniques de débogage telles que les breakpoints, l'inspection des variables et les journaux de la console. Mais avez-vous déjà pensé à inclure des sorties de débogage directement dans votre code ? Cela peut sembler étrange au premier abord, mais croyez-moi, c'est une pratique qui peut grandement faciliter votre processus de débogage.

## Comment faire

Pour imprimer des sorties de débogage dans votre code TypeScript, vous pouvez utiliser la méthode `console.log()`. Il suffit de placer cette fonction avec un argument dans votre code pour afficher la valeur de cet argument dans la console. Par exemple :

```TypeScript
console.log("La valeur de x est : ", x);
```

Vous pouvez également utiliser des paramètres de formatage pour afficher des valeurs plus complexes ou pour ajouter des informations supplémentaires. Par exemple :

```TypeScript
console.log(`L'utilisateur ${username} a un solde de ${balance}€.`);
```

Le résultat de ces lignes de code sera une sortie dans la console qui ressemble à ceci :

```
La valeur de x est : 5
L'utilisateur John Doe a un solde de 150€.
```

## Plongée en profondeur

La méthode `console.log()` peut sembler simple, mais elle peut être très utile pour le débogage de votre code. Vous pouvez non seulement afficher des valeurs de variables, mais aussi des informations de débogage telles que des messages d'erreur ou des résultats de fonctions. Vous pouvez également utiliser d'autres méthodes de la console, comme `console.warn()` ou `console.error()`, pour différencier les types de sorties.

Il est également important de noter que vous pouvez désactiver ces sorties de débogage avant de déployer votre code en production. Vous pouvez le faire en configurant votre environnement avec des variables d'environnement ou en utilisant des bibliothèques de débogage spéciales qui vous permettent de les activer ou de les désactiver à tout moment.

## Voir aussi

- [Site officiel de TypeScript](https://www.typescriptlang.org/)
- [Documentation sur la console de Chrome](https://developer.chrome.com/docs/devtools/console/)
- [Article sur la méthode console.log() en JavaScript](https://www.digitalocean.com/community/tutorials/how-to-use-the-console-log-function-in-javascript)