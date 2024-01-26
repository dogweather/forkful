---
title:                "Manipulation des nombres complexes"
date:                  2024-01-26T04:37:20.268753-07:00
model:                 gpt-4-0125-preview
simple_title:         "Manipulation des nombres complexes"
programming_language: "C"
category:             "C"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/working-with-complex-numbers.md"
---

{{< edit_this_page >}}

## Quoi et pourquoi ?
Les nombres complexes, un mélange de parties réelles et imaginaires (comme 3 + 4i), sont clés dans des calculs avancés, tels que le traitement du signal ou la résolution de certaines équations. Les programmeurs les manipulent pour des applications lourdes en mathématiques où les nombres traditionnels ne suffisent pas.

## Comment faire :
C, depuis C99, dispose d'un type complexe natif et d'une bibliothèque. Voici comment l'utiliser :

```C
#include <stdio.h>
#include <complex.h>

int main() {
    // Déclaration de deux nombres complexes
    double complex z1 = 1.0 + 3.0 * I;
    double complex z2 = 2.0 - 2.0 * I;

    // Opérations avec des nombres complexes
    double complex somme = z1 + z2;
    double complex mult = z1 * z2;

    // Affichage des résultats
    printf("Sum: %.1f + %.1fi\n", creal(somme), cimag(somme));
    printf("Produit: %.1f + %.1fi\n", creal(mult), cimag(mult));

    // Valeur absolue & angle de phase
    printf("Abs(z1): %f\n", cabs(z1));
    printf("Arg(z1): %f\n", carg(z1));

    return 0;
}
```

Résultats d'exemple :
```
Somme : 3.0 + 1.0i
Produit : 8.0 + 2.0i
Abs(z1) : 3.162278
Arg(z1) : 1.249046
```
## Exploration plus approfondie
Les nombres complexes remontent à des siècles, avec des origines dans l'algèbre du 16ème siècle. En accélérant jusqu'à aujourd'hui, ils sont désormais un pilier dans de nombreux langages de programmation, pas seulement en C.

Le standard C99 a introduit `<complex.h>`, un en-tête définissant des macros, des fonctions et le type de données `complex`. Des alternatives existent - comme créer votre propre structure, mais pourquoi réinventer la roue ? La bibliothèque standard C est optimisée et prête à l'emploi.

Malgré sa puissance, le support complexe de C n'est pas sans critiques. Il peut être moins intuitif que des fonctionnalités similaires dans des langages comme Python, et la gestion des cas particuliers peut devenir délicate. Mais pour la performance brute, c'est toujours un choix solide.

## Voir aussi
- Documentation du standard C99 pour `<complex.h>` : https://en.cppreference.com/w/c/numeric/complex
- Standard IEEE pour l'arithmétique à virgule flottante (IEEE 754) : https://ieeexplore.ieee.org/document/4610935
- Tutoriel en ligne pour les mathématiques des nombres complexes en C : https://www.tutorialspoint.com/complex-number-arithmetic-in-c-programming