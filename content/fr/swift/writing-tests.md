---
title:                "Swift: Écriture de tests"
simple_title:         "Écriture de tests"
programming_language: "Swift"
category:             "Swift"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/swift/writing-tests.md"
---

{{< edit_this_page >}}

## Pourquoi

Dans le monde de la programmation, écrire des tests est devenu une pratique essentielle pour garantir la qualité et la stabilité du code. En utilisant Swift, nous pouvons facilement écrire des tests pour nos applications iOS ou macOS.

## Comment faire

Pour écrire des tests en Swift, nous pouvons utiliser le framework XCTest intégré dans Xcode. Jetons un coup d'œil à un exemple de code pour tester une fonction de calcul de moyenne :

```
Swift func calculateAverage(numbers: [Int]) -> Double { // fonction pour calculer la moyenne let sum = numbers.reduce(0, +) // somme des nombres dans le tableau return Double(sum) / Double(numbers.count) // calcul de la moyenne }

```

```
XCTest // test unitaire pour la fonction de calcul de moyenne func testCalculateAverage() { let numbers = [5, 10, 15] // tableau de nombres let average = calculateAverage(numbers: numbers) // appel de la fonction XCTAssertEqual(average, 10, "La moyenne doit être égale à 10") // vérification de l'égalité } ```` 

Le résultat de ce test sera "Réussi", car la moyenne calculée est effectivement égale à 10.

## Plongée profonde

Lorsque nous écrivons des tests en Swift, il est important de suivre les principes du "test-driven development" (TDD) : écrire d'abord les tests avant le code lui-même. Cela nous permet de mieux comprendre les fonctionnalités à implémenter et de nous assurer que notre code est testable et flexible.

De plus, il est important de couvrir tous les cas possibles dans nos tests, y compris les cas d'erreurs et les cas de bords. En utilisant des techniques telles que les mocks et les fakes, nous pouvons également tester des parties spécifiques de notre code, sans avoir à exécuter l'application entière.

## Voir aussi

- [Documentation XCTest](https://developer.apple.com/documentation/xctest)
- [Introduction au Test-Driven Development](https://martinfowler.com/bliki/TestDrivenDevelopment.html)
- [Test Doubles in Swift: Mocks, Stubs, Fakes, Spies and Dummies](https://matteomanferdini.com/test-doubles-swift-mocks-stubs/)
- [Introduction à Xcode pour les tests unitaires en Swift](https://medium.com/xcblog/xcode-tdd-swift-unit-tests-3d164f3e3ca4)