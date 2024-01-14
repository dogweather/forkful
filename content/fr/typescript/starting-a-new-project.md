---
title:                "TypeScript: Lancer un nouveau projet"
programming_language: "TypeScript"
category:             "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Pourquoi

Avant de commencer un nouveau projet de programmation en TypeScript, vous pourriez vous demander pourquoi vous devriez vous lancer dans cette aventure. La réponse est simple : TypeScript est un langage de programmation qui combine les avantages de la typage statique et de la flexibilité du JavaScript. Il offre une meilleure lisibilité du code, une détection d'erreur plus rapide et une maintenance plus facile à long terme. 

## Comment faire

Pour commencer un nouveau projet en TypeScript, vous devez tout d'abord installer le compilateur TypeScript sur votre ordinateur. Vous pouvez le faire en utilisant npm (Node Package Manager) avec la commande `npm install -g typescript`. Une fois l'installation terminée, vous pouvez créer un nouveau fichier avec l'extension `.ts` et y écrire votre code TypeScript. Voici un exemple de code qui imprime "Bonjour le monde" dans la console : 

```TypeScript 
let message: string = "Bonjour le monde";
console.log(message);
```

Lorsque vous exécutez ce code en utilisant la commande `tsc <nom du fichier>.ts`, il sera compilé en JavaScript et vous pourrez ensuite l'exécuter en utilisant `node <nom du fichier>.js`.

## Plongez plus profondément

En commençant un nouveau projet en TypeScript, vous verrez rapidement que la syntaxe peut sembler familière si vous avez une expérience en JavaScript. Cependant, il y a quelques différences importantes à noter. Par exemple, l'utilisation de la déclaration de type `let` pour les variables et de la déclaration de types pour les paramètres de fonction. De plus, TypeScript offre une grande variété de types prédéfinis ainsi que la possibilité de créer vos propres types personnalisés.

Il est également important de noter que TypeScript permet d'utiliser des fonctionnalités avancées telles que la programmation orientée objet et les annotations de type, ce qui peut rendre votre code plus robuste et maintenable. Vous pouvez également utiliser des bibliothèques JavaScript existantes dans vos projets TypeScript en utilisant des définitions de types ou des modules externes.

## Voir aussi

- [Guide officiel de TypeScript](https://www.typescriptlang.org/docs/handbook/typescript-in-5-minutes.html)
- [Documentation de npm pour l'installation de TypeScript](https://www.npmjs.com/package/typescript)
- [Exemples de projets TypeScript sur Github](https://github.com/search?q=typescript+projects)
- [Vidéos de tutoriels sur TypeScript](https://www.youtube.com/results?search_query=typescript+tutorials)