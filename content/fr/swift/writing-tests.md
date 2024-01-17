---
title:                "Écriture de tests"
html_title:           "Swift: Écriture de tests"
simple_title:         "Écriture de tests"
programming_language: "Swift"
category:             "Swift"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/swift/writing-tests.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi le faire?
Ecrire des tests fait partie intégrante de la pratique de programmation pour les développeurs. Cela signifie écrire des morceaux de code pour vérifier que notre programme fonctionne comme prévu. Les tests permettent de détecter les erreurs avant qu'elles ne se présentent aux utilisateurs et de s'assurer que le code fonctionne correctement même après de futurs changements.

## Comment faire:
Voici un exemple simple de test en Swift pour une fonction qui calcule la somme de deux nombres :
```Swift
func addition(_ a: Int, _ b: Int) -> Int {
  return a + b
}

func testAddition() {
  let result = addition(2, 3)
  assert(result == 5, "La somme de 2 et 3 devrait être égale à 5, mais le résultat obtenu est \(result).")
}

testAddition()
```
Nous pouvons ainsi vérifier si notre fonction additionne correctement deux nombres en nous assurant que le résultat est celui attendu. Dans ce cas, la somme de 2 et 3 doit être égale à 5.

## Plongée en profondeur:
Les tests ont évolué au fil du temps, mais leur but principal est resté le même: s'assurer que notre code fonctionne correctement. Il existe différentes approches pour écrire des tests, mais celle utilisée en Swift est appelée "Test-Driven Development" (TDD). Cela signifie que nous écrivons les tests avant d'écrire le code réel, ce qui nous permet de connaître à l'avance le comportement attendu du programme. Bien sûr, il existe d'autres approches comme "Behavior-Driven Development" (BDD) et "Acceptance Test-Driven Development" (ATDD). Chacune a ses avantages et ses inconvénients, mais la clé est de toujours écrire des tests pour notre code.

## Voir aussi:
Si vous souhaitez en savoir plus sur les tests en programmation Swift, voici quelques ressources utiles :
- La documentation officielle sur les tests en Swift : https://developer.apple.com/documentation/xctest
- Une vidéo de WWDC sur le Test-Driven Development en Swift : https://developer.apple.com/videos/play/wwdc2018/404/
- Un article sur les différents types de tests en Swift : https://www.hackingwithswift.com/articles/90/different-types-of-unit-test-in-swift