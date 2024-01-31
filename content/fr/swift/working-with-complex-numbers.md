---
title:                "Manipulation des nombres complexes"
date:                  2024-01-26T04:45:39.345835-07:00
model:                 gpt-4-0125-preview
simple_title:         "Manipulation des nombres complexes"

category:             "Swift"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/swift/working-with-complex-numbers.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Les nombres complexes ont une partie réelle et une partie imaginaire (comme 3 + 4i). Les programmeurs les utilisent en Swift pour des tâches telles que le traitement du signal, la résolution de certains problèmes mathématiques et la simulation physique.

## Comment faire :
Swift n'a pas de support intégré pour les nombres complexes, mais nous pouvons développer le nôtre :

```Swift
struct NombreComplexe {
    var reel: Double
    var imaginaire: Double
    
    func add(_ autre: NombreComplexe) -> NombreComplexe {
        return NombreComplexe(reel: reel + autre.reel, imaginaire: imaginaire + autre.imaginaire)
    }
    
    // Méthodes supplémentaires comme la soustraction, la multiplication, etc.
}

let premier = NombreComplexe(reel: 2, imaginaire: 3)
let second = NombreComplexe(reel: 1, imaginaire: 4)
let resultat = premier.add(second)
print("Résultat: \(resultat.reel) + \(resultat.imaginaire)i")
// Exemple de sortie : Résultat: 3.0 + 7.0i
```

## Plongée profonde
Les nombres complexes sont apparus au 16ème siècle dans les équations algébriques. Ils sont essentiels en mécanique quantique, en théorie de contrôle et dans de nombreux autres domaines. La Swift d'Apple n'a pas de bibliothèque standard pour les nombres complexes, contrairement à des langues comme Python ou C++. Les alternatives pour développer le vôtre incluent l'utilisation du package Numerics qui inclut le support des nombres complexes ou l'enveloppement de la bibliothèque complexe de C++ avec l'interopérabilité de Swift.

## Voir aussi
- Swift Numerics : [https://github.com/apple/swift-numerics](https://github.com/apple/swift-numerics)
