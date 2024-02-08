---
title:                "Rédaction de tests"
aliases:
- fr/swift/writing-tests.md
date:                  2024-02-03T19:32:12.877505-07:00
model:                 gpt-4-0125-preview
simple_title:         "Rédaction de tests"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/swift/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Écrire des tests en Swift implique de créer et d'exécuter du code qui vérifie la justesse d'autres unités de code dans votre application. Les programmeurs le font pour assurer la fiabilité, détecter les bugs tôt dans le cycle de développement, et faciliter le refactoring du code futur sans conséquences involontaires.

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
