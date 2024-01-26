---
title:                "Génération de nombres aléatoires"
date:                  2024-01-20T17:48:48.682277-07:00
model:                 gpt-4-1106-preview
simple_title:         "Génération de nombres aléatoires"
programming_language: "C++"
category:             "C++"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Quoi et Pourquoi ?

Générer des nombres aléatoires en C++ consiste à produire des valeurs imprévisibles à chaque exécution. Les programmeurs utilisent cette fonctionnalité pour des choses comme les jeux, les simulations et la sécurité informatique.

## Comment faire :

```C++
#include <iostream>
#include <random>

int main() {
    // Créez un générateur - ici nous utilisons mt19937
    std::mt19937 generateur(std::random_device{}());
    // Définissez la distribution et l'intervalle - ici 1 à 6 pour simuler un dé
    std::uniform_int_distribution<int> distribution(1, 6);

    // Générez et affichez le nombre aléatoire
    int nombre_aleatoire = distribution(generateur);
    std::cout << "Nombre aléatoire: " << nombre_aleatoire << std::endl;

    return 0;
}
```

Sortie d'exemple:
```
Nombre aléatoire: 4
```

## Plongée Profonde

Jadis, C++ s'appuyait sur la fonction `rand()`, mais avait plusieurs inconvénients, comme des distributions non uniformes et une faible qualité de l'aléatoirité. Depuis C++11, `<random>` est là. Il offre des générateurs de qualité et des distributions variées. Le `std::mt19937` est un bon choix de générateur; il est basé sur l'algorithme Mersenne Twister, connu pour sa longue période et sa performance. N'oubliez pas de l'initialiser avec un `std::random_device` pour de meilleurs résultats. Il existe d'autres distributions comme `std::normal_distribution` pour les besoins différents.

## Voir Aussi

- Documentation de C++ sur `<random>`: https://en.cppreference.com/w/cpp/header/random
- Article sur les générateurs de nombres pseudo-aléatoires: https://en.cppreference.com/w/cpp/numeric/random
- Le Mersenne Twister: https://en.wikipedia.org/wiki/Mersenne_Twister
