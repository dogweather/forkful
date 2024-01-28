---
title:                "Arrondir les nombres"
date:                  2024-01-26T03:43:24.560143-07:00
model:                 gpt-4-0125-preview
simple_title:         "Arrondir les nombres"
programming_language: "C++"
category:             "C++"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/rounding-numbers.md"
---

{{< edit_this_page >}}

## Quoi et pourquoi ?
Arrondir des nombres signifie ajuster une valeur à son entier le plus proche ou à la précision spécifiée. Les développeurs le font pour simplifier, se conformer aux contraintes du monde réel, ou améliorer la performance en éliminant la précision superflue.

## Comment faire :
C++ offre plusieurs façons d'arrondir les nombres, comme `floor()`, `ceil()`, et `round()` :

```C++
#include <iostream>
#include <cmath> // pour les fonctions d'arrondi

int main() {
    double num = 3.14;

    std::cout << "floor: " << std::floor(num) << "\n"; // Résultat : floor: 3
    std::cout << "ceil: " << std::ceil(num) << "\n";   // Résultat : ceil: 4
    std::cout << "round: " << std::round(num) << "\n"; // Résultat : round: 3

    // Pour une précision fixe, comme l'arrondi à deux décimales :
    double precise_num = 3.146;
    double multiplicateur = 100.0;
    double arrondi = std::round(precise_num * multiplicateur) / multiplicateur;

    std::cout << "arrondi à deux décimales : " << arrondi << "\n"; // Résultat : arrondi à deux décimales : 3.15

    return 0;
}
```

## Plongée en profondeur
Avant C++11, l'arrondi reposait sur des techniques manuelles ou des bibliothèques non standard. Aujourd'hui, `<cmath>` fournit des méthodes robustes. `floor()` arrondit vers le bas, `ceil()` arrondit vers le haut, tandis que `round()` va à l'entier le plus proche, gérant même les cas de partage équitable (cas de 0,5) en arrondissant au nombre pair.

Comprendre le comportement de ces fonctions est crucial ; par exemple, les nombres négatifs pourraient vous poser problème (`std::round(-2.5)` donne `-2.0`).

Des alternatives ? Convertir en un int après avoir ajouté 0.5 pour les nombres positifs était un vieux truc mais cela échoue avec les nombres négatifs et ce n'est pas indifférent au type. Des bibliothèques comme Boost peuvent offrir des approches plus nuancées, tandis que les extensions de langage ou les intrinsèques du compilateur peuvent optimiser pour un matériel spécifique.

## Voir aussi
- Référence C++ pour `<cmath>` : https://en.cppreference.com/w/cpp/header/cmath
- Norme IEEE pour l'arithmétique à virgule flottante (IEEE 754) : https://ieeexplore.ieee.org/document/4610935
- Bibliothèque de conversion numérique Boost : https://www.boost.org/doc/libs/release/libs/numeric/conversion/
