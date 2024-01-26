---
title:                "Arrondir les nombres"
date:                  2024-01-26T03:42:50.960333-07:00
model:                 gpt-4-0125-preview
simple_title:         "Arrondir les nombres"
programming_language: "C"
category:             "C"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/rounding-numbers.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Arrondir des nombres consiste à supprimer les chiffres au-delà d'un certain point, tout en ajustant éventuellement le dernier chiffre conservé. Les programmeurs arrondissent pour réduire la précision lorsque les valeurs exactes ne sont pas nécessaires, gérer les erreurs de point flottant, ou préparer les nombres pour un affichage convivial.

## Comment faire :
En C, vous utiliseriez typiquement les fonctions `floor()`, `ceil()`, ou `round()`. Voici une démonstration rapide :

```C
#include <stdio.h>
#include <math.h>

int main() {
    double num = 3.14159;
    double num_floor = floor(num);
    double num_ceil = ceil(num);
    double num_round = round(num);

    printf("Plancher: %.2f\n", num_floor); // Plancher: 3.00
    printf("Plafond: %.2f\n", num_ceil);   // Plafond: 4.00
    printf("Arrondi: %.2f\n", num_round); // Arrondi: 3.00
    return 0;
}
```

Pour plus de contrôle, comme l'arrondissement à un endroit spécifique, vous multipliez, arrondissez et divisez :

```C
double roundToPlace(double num, int place) {
    double scale = pow(10.0, place);
    return round(num * scale) / scale;
}

// ...

double num = 3.14159;
double num_rounded = roundToPlace(num, 2);
printf("Arrondi à 2 décimales : %.2f\n", num_rounded); // Arrondi à 2 décimales : 3.14
```

## Exploration Approfondie
Autrefois, arrondir signifiait souvent un processus manuel—une tâche lourde juste avec un stylo et du papier. Avec l'informatique, nous avons automatisé cela, mais l'arithmétique à virgule flottante a introduit des nuances en raison de sa nature binaire, où certains nombres ne peuvent pas être représentés exactement.

Les alternatives au standard d’arrondi incluent la troncation (simplement en supprimant les chiffres supplémentaires) ou l’arrondi des banquiers, qui arrondit au nombre pair le plus proche lorsqu’on est exactement entre deux valeurs, réduisant le biais dans les calculs répétés.

L'implémentation devient délicate lorsque vous devez arrondir des nombres de précision arbitraire ou gérer des cas spéciaux comme l'infini, les NaNs signalant, ou les valeurs subnormales. Les fonctions de la bibliothèque standard C gèrent les bases, mais si vous avez besoin d'arrondir des décimales de manière personnalisée, vous aurez besoin de plus que `math.h`.

## Voir Aussi
- [Documentation de `<math.h>`](https://en.cppreference.com/w/c/numeric/math)
- [Arithmétique à virgule flottante](https://fr.wikipedia.org/wiki/Arithm%C3%A9tique_%C3%A0_virgule_flottante)
- [Les pièges de la vérification des calculs à virgule flottante](https://dl.acm.org/doi/10.1145/1186736.1186737)