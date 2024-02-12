---
title:                "Arrondir les nombres"
aliases:
- /fr/swift/rounding-numbers.md
date:                  2024-01-26T03:46:49.451668-07:00
model:                 gpt-4-0125-preview
simple_title:         "Arrondir les nombres"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/swift/rounding-numbers.md"
---

{{< edit_this_page >}}

## Quoi et pourquoi ?

Arrondir des nombres signifie approximer une valeur numérique à une précision spécifique, typiquement pour éliminer les décimales indésirables. Les programmeurs arrondissent pour gérer la mémoire, améliorer la lisibilité et répondre aux contraintes spécifiques d'un domaine, comme les contraintes monétaires.

## Comment faire :

Swift offre plusieurs manières d'arrondir les nombres. En voici un aperçu :

```Swift
let original = 3.14159

// Arrondissement standard
let standardRounded = round(original) // 3.0

// Arrondissement à un nombre spécifique de décimales
let decimalRounded = Double(round(original * 1000) / 1000) // 3.142

// Arrondissement à l'inférieur
let roundedDown = floor(original) // 3.0

// Arrondissement à la hausse
let roundedUp = ceil(original) // 4.0

print("Standard: \(standardRounded), Decimal: \(decimalRounded), Inférieur: \(roundedDown), Supérieur: \(roundedUp)")
```

Sortie : `Standard: 3.0, Decimal: 3.142, Inférieur: 3.0, Supérieur: 4.0`

## Plongée profonde

Historiquement, l'arrondissement est un concept mathématique antérieur aux ordinateurs, essentiel dans le commerce et la science. Le cadre `Foundation` de Swift offre des fonctionnalités d'arrondissement complètes :

- `round(_: )` est le bon vieux arrondissement à la demi-supérieure.
- `floor(_: )` et `ceil(_: )` gèrent l'arrondissement directionnel.
- `rounded(.up/.down/.toNearestOrAwayFromZero)` offre un contrôle plus fin avec l'énumération des règles d'arrondissement.

Soyez conscient du type `Decimal` pour des calculs financiers précis, qui évite les erreurs de point flottant. Explorez également `NSDecimalNumber` pour la compatibilité avec Objective-C.

## Voir aussi

- Norme IEEE pour l'arithmétique à virgule flottante (IEEE 754) : [IEEE 754](https://ieeexplore.ieee.org/document/4610935)
