---
title:                "Rédaction de tests"
html_title:           "Arduino: Rédaction de tests"
simple_title:         "Rédaction de tests"
programming_language: "Swift"
category:             "Swift"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/swift/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why?
On écrit des tests pour vérifier que notre code fait bien ce qu'on lui demande. C'est essentiel pour éviter les bugs et garantir la qualité à long terme.

## How to:
En Swift, on utilise XCTest pour écrire des tests. Voici un exemple simple :

```Swift
import XCTest
@testable import MonSuperProjet

class TestsDeMonProjet: XCTestCase {
    func testExample() {
        let result = MaClasse().maFonction()
        XCTAssertEqual(result, "Attendu")
    }
}

// Résultat du test :
// Test Case '-[TestsDeMonProjet testExample]' passed (0.001 seconds).
```

## Deep Dive
XCTest, intégré depuis Swift 1.0, est le cadre de test de prédilection. Avant XCTest, OCUnit était utilisé dans Objective-C. Alternatives : Quick/Nimble pour une syntaxe différente. Pour les implémentations, on distingue les tests unitaires (petites fonctions) des tests UI (interaction avec l'utilisateur).

## See Also
- Documentation Apple XCTest : [https://developer.apple.com/documentation/xctest](https://developer.apple.com/documentation/xctest)
- Article sur Quick/Nimble : [https://github.com/Quick/Nimble](https://github.com/Quick/Nimble)
