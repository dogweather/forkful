---
title:                "Écriture des tests"
html_title:           "Javascript: Écriture des tests"
simple_title:         "Écriture des tests"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/writing-tests.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi le faire?

Ecrire des tests en Javascript est une pratique courante parmi les programmeurs pour garantir le bon fonctionnement de leur code. Les tests sont des morceaux de code qui vérifient si une partie spécifique du code fonctionne correctement. Cela permet aux programmeurs de détecter et de corriger les erreurs avant qu'elles ne deviennent des problèmes pour les utilisateurs.

## Comment procéder:

Pour écrire des tests en Javascript, il est important de comprendre comment les tests sont structurés. Voici un exemple simple de test unitaire pour une fonction nommée "add" qui additionne deux nombres:

```Javascript
function add(a, b) {
  return a + b;
}

// test unitaire pour la fonction "add"
test("test add function", () => {
  const sum = add(5, 10);
  expect(sum).toBe(15); // vérifie si le résultat est bien égal à 15
});
```

Dans cet exemple, nous créons une fonction "add" qui prend deux paramètres et renvoie la somme de ces deux nombres. Pour tester cette fonction, nous utilisons la fonction "test" et la méthode "expect" pour vérifier si le résultat de la fonction est bien égal à 15. Si c'est le cas, le test est réussi. Sinon, le test échoue et nous devons réviser notre fonction "add" pour qu'elle fonctionne correctement.

## Plongée en profondeur:

La pratique de l'écriture de tests en Javascript a pris de l'importance avec l'avènement du développement piloté par les tests (TDD). Cette approche consiste à écrire d'abord des tests avant d'écrire le code réel, ce qui permet de s'assurer que le produit final fonctionne correctement dès le début. Il existe également d'autres frameworks de tests populaires en Javascript tels que Jest, Mocha et Jasmine.

En plus des tests unitaires, il existe également des tests d'intégration qui vérifient le bon fonctionnement de différentes parties d'une application ensemble. Certains outils, comme Cypress, permettent même d'effectuer des tests end-to-end pour vérifier le comportement d'une application dans son ensemble.

Il est également important de noter que l'écriture de tests ne garantit pas à 100% un code sans erreurs, mais cela aide grandement à minimiser les problèmes et à améliorer la qualité de votre code.

## Voir aussi:

- [Comparaison des frameworks de tests en Javascript](https://blog.bitsrc.io/which-javascript-testing-library-should-i-use-a-comparison-guide-eventloop-ca5360ed2967)
- [Guide de Cypress pour les tests end-to-end](https://docs.cypress.io/guides/overview/why-cypress.html)