---
title:                "Générer des nombres aléatoires"
html_title:           "Elixir: Générer des nombres aléatoires"
simple_title:         "Générer des nombres aléatoires"
programming_language: "C++"
category:             "C++"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/generating-random-numbers.md"
---

{{< edit_this_page >}}

---
titre: Génération de Nombres Aléatoires en C++

## Qu'est-ce et Pourquoi?

La génération de nombres aléatoires se réfère à la création de nombres qui ne suivent aucun ordre prévisible. C'est essentiel dans la programmation pour des situations comme les jeux, les simulateurs, les tests de stress, et plus.

## Comment faire :

Voici comment vous pouvez générer des nombres aléatoires en utilisant C++.

```C++
#include <iostream>
#include <cstdlib> 
#include <ctime> 

int main() 
{ 
    srand((unsigned) time(0)); 
    int random_integer = rand(); 
    std::cout << "Random Number: " << random_integer << std::endl; 

    return 0; 
}
```
Dans ce code, ```srand((unsigned) time(0))``` initialise le générateur de nombres aléatoires avec la valeur actuelle du temps du système. ```rand()``` est ensuite utilisée pour générer un nombre aléatoire.

## Plongée profonde

Historiquement, C++ utilisait la fonction ```rand()```. Cependant, dans le standard moderne de C++, ils recommandent d'utiliser le générateur de nombres aléatoires de la bibliothèque ```<random>```. Voici comment vous pouvez l'utiliser:

```C++
#include <iostream>
#include <random>

int main() {
    std::mt19937 generator(std::random_device{}());
    std::uniform_int_distribution<int> distribution(1,100);
    int random = distribution(generator);
    std::cout << "Random Number Between 1 and 100: " << random << std::endl;
    return 0;
}
```
Dans ce code, ```std::mt19937``` est un générateur de nombres pseudo-aléatoires de type Mersenne Twister. ```std::uniform_int_distribution``` produit des nombres aléatoires dans un range spécifique.

Une alternative possible à la génération de nombres aléatoires est l'utilisation de séquences pseudo-aléatoires. Cependant, les séquences pseudo-aléatoires ne sont pas réellement aléatoires, elles suivent seulement un motif difficile à prédire.

## Voir aussi

Voici quelques liens vers des ressources supplémentaires :

1. [cplusplus.com - \<random\>](http://www.cplusplus.com/reference/random/)
2. [cppreference.com - Random number generation library](https://en.cppreference.com/w/cpp/numeric/random)
3. [GeeksforGeeks - Random number generator in arbitrary probability distribution fashion](https://www.geeksforgeeks.org/random-number-generator-in-arbitrary-probability-distribution-fashion/)

---