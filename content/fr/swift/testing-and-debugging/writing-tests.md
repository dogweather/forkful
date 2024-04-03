---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:32:12.877505-07:00
description: "Comment faire : Swift prend en charge les tests via son framework XCTest,\
  \ qui est int\xE9gr\xE9 dans Xcode. Vous pouvez \xE9crire des tests unitaires pour\
  \ v\xE9rifier\u2026"
lastmod: '2024-03-13T22:44:58.226452-06:00'
model: gpt-4-0125-preview
summary: "Swift prend en charge les tests via son framework XCTest, qui est int\xE9\
  gr\xE9 dans Xcode."
title: "R\xE9daction de tests"
weight: 36
---

## Comment faire :
Swift prend en charge les tests via son framework XCTest, qui est intégré dans Xcode. Vous pouvez écrire des tests unitaires pour vérifier des parties individuelles de votre code, par exemple, une fonction qui calcule la somme de deux nombres.

```swift
import XCTest
@testable import YourApp

class VosTestsApp: XCTestCase {

    func testSomme() {
        let resultat = Calculatrice().somme(a: 1, b: 2)
        XCTAssertEqual(resultat, 3, "La fonction somme n'a pas retourné la valeur attendue.")
    }
}
```

Pour exécuter ce test, vous appuyeriez typiquement sur Commande-U dans Xcode. Le résultat dans le navigateur de tests Xcode vous indiquera si le test a réussi ou échoué.

Par exemple, un résultat de test réussi :
```
Test Case '-[VosTestsApp testSomme]' passed (0,005 secondes).
```

Pour des scénarios de tests plus avancés, vous pourriez adopter des bibliothèques tierces telles que Quick/Nimble, qui offrent une syntaxe plus expressive pour écrire des tests.

Avec Quick/Nimble, vous pourriez écrire le même test ainsi :

```swift
// Ajoutez Quick et Nimble à votre gestionnaire de package Swift ou utilisez CocoaPods/Carthage pour les installer
import Quick
import Nimble
@testable import YourApp

class SpecCalculatrice: QuickSpec {
    override func spec() {
        describe("Calculatrice") {
            context("lors du calcul de la somme") {
                it("devrait retourner la somme correcte") {
                    let calculatrice = Calculatrice()
                    expect(calculatrice.somme(a: 1, b: 2)).to(equal(3))
                }
            }
        }
    }
}
```

L'exécution de ce test vous donnerait un résultat similaire dans votre console de test ou le journal de votre outil CI/CD, indiquant si le test a réussi ou échoué, avec un format plus lisible pour décrire les tests et les attentes.
