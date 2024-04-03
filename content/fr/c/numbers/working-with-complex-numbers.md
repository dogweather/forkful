---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:14:35.759511-07:00
description: "Comment faire : En C, les nombres complexes sont pris en charge par\
  \ la biblioth\xE8que standard, sp\xE9cifiquement `<complex.h>`. Pour les utiliser,\
  \ d\xE9clarez\u2026"
lastmod: '2024-03-13T22:44:58.364838-06:00'
model: gpt-4-0125-preview
summary: "En C, les nombres complexes sont pris en charge par la biblioth\xE8que standard,\
  \ sp\xE9cifiquement `<complex.h>`."
title: Travailler avec des nombres complexes
weight: 14
---

## Comment faire :
En C, les nombres complexes sont pris en charge par la bibliothèque standard, spécifiquement `<complex.h>`. Pour les utiliser, déclarez des variables avec le type `double complex` (ou `float complex` pour une précision simple). Voici comment effectuer les opérations de base :

```c
#include <stdio.h>
#include <complex.h>

int main() {
    double complex z1 = 1.0 + 2.0*I; // Déclarer un nombre complexe 1+2i
    double complex z2 = 1.0 - 2.0*I; // Déclarer un autre nombre complexe 1-2i

    // Addition
    double complex sum = z1 + z2;
    printf("Somme : %.2f + %.2fi\n", creal(sum), cimag(sum)); // Sortie : Somme : 2.00 + 0.00i

    // Multiplication
    double complex product = z1 * z2;
    printf("Produit : %.2f + %.2fi\n", creal(product), cimag(product)); // Sortie : Produit : 5.00 + 0.00i

    // Conjugué complexe
    double complex conjugate = conj(z1);
    printf("Conjugué de z1 : %.2f + %.2fi\n", creal(conjugate), cimag(conjugate)); // Sortie : Conjugué de z1 : 1.00 - 2.00i

    // Magnitude
    double magnitude = cabs(z1);
    printf("Magnitude de z1 : %.2f\n", magnitude); // Sortie : Magnitude de z1 : 2.24

    // Phase
    double phase = carg(z1);
    printf("Phase de z1 : %.2f\n", phase); // Sortie en radians

    return 0;
}
```
Notez que `I` est une constante représentant l'unité imaginaire dans `<complex.h>`. Des fonctions comme `creal()` et `cimag()` extraient les parties réelle et imaginaire respectivement, tandis que `conj()` calcule le conjugué complexe. Pour la magnitude et la phase (argument) des nombres complexes, `cabs()` et `carg()` sont utilisés.

## Approfondissement
Le support des nombres complexes en C est relativement récent, ayant été normalisé dans C99. Avant cela, l'arithmétique des nombres complexes en C était laborieuse, nécessitant souvent des structures de données personnalisées et des fonctions. L'inclusion de `<complex.h>` et des types de données complexes a fourni un important coup de pouce aux capacités du langage pour les applications scientifiques et d'ingénierie. Cependant, il convient de noter que certains langages, comme Python, offrent un support plus intuitif pour les nombres complexes à travers des types de données intégrés et un ensemble plus riche de fonctions de bibliothèque. Malgré cela, la performance et le contrôle offerts par le C en font un choix privilégié pour les tâches de calcul haute performance, même s'il faut faire face à une syntaxe légèrement plus verbeuse pour l'arithmétique complexe.
