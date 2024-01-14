---
title:    "Javascript: Écrire des tests"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## Pourquoi

L'écriture de tests est un aspect très important de la programmation en Javascript. Non seulement elle vous permet de vérifier si votre code fonctionne correctement, mais elle vous permet également de détecter et de résoudre rapidement les erreurs. Cela vous fait gagner du temps et vous évite des problèmes à l'avenir.

## Comment faire

Pour écrire des tests en Javascript, vous devez utiliser un framework de test tel que Mocha ou Jest. Vous pouvez également utiliser l'outil de test intégré de votre environnement de développement (IDE). Les tests doivent être écrits dans des fichiers séparés et doivent être organisés en fonction des fonctionnalités qu'ils testent.

Voici un exemple de code pour tester une fonction qui ajoute deux nombres :

```Javascript
// Importer le framework de test
const assert = require('assert');

// Importer la fonction à tester
const sum = require('./sum');

// Décrire le test avec un nom et une fonction
describe('Additionner deux nombres', () => {

  // Écrire des scénarios de test avec des assertions
  it('devrait retourner 4 si les nombres sont 2 et 2', () => {
    assert.equal(sum(2, 2), 4);
  });

  it('devrait retourner 10 si les nombres sont 5 et 5', () => {
    assert.equal(sum(5, 5), 10);
  });
});
```

Vous pouvez exécuter ces tests en utilisant la commande ```npm test``` dans votre terminal. Si tous les tests réussissent, vous verrez un message "0 errors, 2 tests passed".

## Plongée en profondeur

Outre les tests unitaires qui vérifient le bon fonctionnement de fonctions et méthodes individuelles, il est également important d'écrire des tests d'intégration qui vérifient le bon fonctionnement de fonctionnalités plus larges de votre application. Vous pouvez également mettre en place des tests automatisés pour exécuter vos tests à chaque fois que vous effectuez des modifications dans votre code.

De plus, vous pouvez utiliser des outils tels que Istanbul pour mesurer la couverture de code de vos tests, vous permettant ainsi de savoir quelle partie de votre code n'est pas encore testée et pourrait potentiellement causer des problèmes.

## Voir aussi

- [Introduction à l'écriture de tests en Javascript](https://blog.devmountain.com/intro-to-javascript-testing/)
- [Documentation de Mocha](https://mochajs.org/)
- [Documentation de Jest](https://jestjs.io/)
- [Documentation de Istanbul](https://istanbul.js.org/)