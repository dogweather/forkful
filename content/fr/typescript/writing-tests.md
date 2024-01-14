---
title:    "TypeScript: Écrire des tests"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## Pourquoi

Ecrire des tests est un aspect essentiel de la programmation en TypeScript. Cela permet de s'assurer que notre code fonctionne correctement et de détecter rapidement d'éventuels bugs. De plus, cela facilite la collaboration avec d'autres développeurs en fournissant une documentation claire et précise du fonctionnement du code.

## Comment faire

Pour commencer, il est important de comprendre la syntaxe de base pour écrire des tests en TypeScript. Voici un exemple de test simple qui vérifie qu'une fonction ajoute correctement deux nombres:

```TypeScript
function addition(a: number, b: number): number {
  return a + b;
}

test("addition", () => {
  expect(addition(2, 3)).toBe(5);
});
```

Dans cet exemple, nous utilisons la fonction `test` fournie par la librairie de tests `jest` pour définir notre test. Nous passons en paramètre le nom du test ainsi qu'une fonction qui exécute notre code et vérifie si le résultat est conforme à nos attentes en utilisant la fonction `expect` également fournie par `jest`.

Nous pouvons également utiliser l'opérateur `toBe` pour vérifier l'égalité stricte entre deux valeurs.

```TypeScript
test("addition avec nombre négatif", () => {
  expect(addition(-2, 5)).toBe(3);
});
```

N'oubliez pas d'installer les librairies nécessaires en utilisant `npm` ou `yarn`, et d'importer les fonctions correspondantes dans votre fichier de test en utilisant `import` ou `require`.

## Deep Dive

Maintenant que nous avons vu un exemple simple, explorons un peu plus en profondeur les tests en TypeScript. Il existe plusieurs librairies de tests disponibles, mais `jest` est l'une des plus populaires et elle offre de nombreuses fonctionnalités utiles telles que la possibilité de mocker des fonctions et des modules pour faciliter les tests unitaires.

De plus, `jest` prend en charge l'utilisation de TypeScript en utilisant un fichier de configuration `jest.config.js` pour définir quelle version de TypeScript utiliser et comment compiler nos fichiers de test.

Pour des tests plus complexes, nous pouvons également utiliser des outils tels que `cypress` pour effectuer des tests end-to-end en simulant des interactions avec notre application.

N'oubliez pas qu'écrire des tests efficaces nécessite une bonne compréhension de votre code et un certain effort, mais les avantages en valent la peine à long terme!

## Voir aussi

- [Documentation de Jest](https://jestjs.io/docs/en/getting-started)
- [Guide de testing en TypeScript](https://www.typescriptlang.org/docs/handbook/intro-to-testing.html)
- [Documentation de Cypress](https://docs.cypress.io/guides/overview/why-cypress.html)