---
title:    "Swift: Écriture des tests"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## Pourquoi 

La mise en place de tests est un aspect essentiel de la programmation en Swift. Les tests sont un moyen efficace de s'assurer que le code fonctionne correctement et qu'il ne présente pas de bogues. Ils permettent également de garantir que les modifications apportées au code n'ont pas d'impact négatif sur les fonctionnalités existantes.

## Comment faire 

Pour commencer à écrire des tests en Swift, il suffit de suivre ces étapes simples : 

1. Créez une classe de test en utilisant la notation ```class NomDeVotreClasseDeTest: XCTestCase```
2. Ajoutez des méthodes de test en utilisant la notation ```func nomDeVotreMethodeDeTest()```
3. Dans chaque méthode de test, utilisez le mot-clé ```XCTAssert``` pour vérifier si une condition est vraie ou fausse. Par exemple : ```XCTAssert(nomDeLaFonctionATester() == true)```
4. Exécutez vos tests en sélectionnant Product > Test dans le menu ou en appuyant sur ```Cmd + U```

Voici un exemple de code de test en Swift : 

```
class AdditionTests: XCTestCase {
    
    // Test si l'addition de 2 nombres fonctionne correctement 
    func testAddition() {
        let a = 5
        let b = 7
        XCTAssertEqual(a + b, 12)
    }
    
    // Test si l'addition de 3 nombres fonctionne correctement 
    func testAdditionTriple() {
        let a = 3
        let b = 9
        let c = 4
        XCTAssertEqual(a + b + c, 16)
    }
}
```

Lors de l'exécution de ces tests, vous devriez voir une ligne verte indiquant que les tests ont réussi ou une ligne rouge indiquant qu'ils ont échoué avec une description de l'erreur.

## Approfondissement 

Il existe plusieurs types de tests en Swift qui peuvent être utilisés pour différents cas de figure. Parmi ceux-ci, on retrouve les tests unitaires qui permettent de tester une petite partie du code, les tests d'intégration qui vérifient si les différentes parties du code fonctionnent ensemble, et les tests d'interface utilisateur qui permettent de s'assurer que l'interface fonctionne correctement.

Il est également important de savoir comment écrire des tests efficaces en utilisant des outils tels que des mocks ou des stubs. Les mocks sont des objets simulés qui remplacent les dépendances du code testé, tandis que les stubs sont des objets avec des valeurs fixes qui remplacent certaines parties du code.

Enfin, il est bon de noter que les tests doivent être écrits au fur et à mesure que le code est développé et qu'ils doivent être exécutés régulièrement pour s'assurer que le code reste fonctionnel.

## Voir aussi 

- [Les bases du développement iOS en Swift](https://openclassrooms.com/fr/courses/4478271-decouvrez-les-bases-du-developpement-ios-en-swift)
- [Guide de démarrage en développement iOS avec Swift](https://developer.apple.com/library/archive/referencelibrary/GettingStarted/DevelopiOSAppsSwift/)
- [Documentation officielle de tests en Swift](https://developer.apple.com/documentation/uikit/testing_with_xcode)