---
title:                "Générer des nombres aléatoires"
html_title:           "C++: Générer des nombres aléatoires"
simple_title:         "Générer des nombres aléatoires"
programming_language: "C++"
category:             "C++"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Pourquoi

Générer des nombres aléatoires peut sembler amusant et sans but précis, mais c'est en réalité une fonctionnalité très utile en programmation. Que ce soit pour simuler des données, créer des jeux ou effectuer des tests, les nombres aléatoires sont souvent indispensables dans le développement de logiciels.

## Comment faire

Pour générer des nombres aléatoires en C++, il existe plusieurs méthodes. La plus simple consiste à utiliser la fonction `rand()` de la bibliothèque standard `cstdlib`. Voici un exemple de code :

```C++
#include <cstdlib> // Inclusion de la bibliothèque standard
#include <iostream>

int main() {
  // Génère un nombre aléatoire entre 0 et 99
  int nombreAleatoire = rand() % 100; 
  std::cout << nombreAleatoire << std::endl;
  return 0;
}
```

Le code ci-dessus utilise également la bibliothèque `iostream` pour afficher le nombre aléatoire généré. On utilise l'opérateur modulo (`%`) pour limiter la plage de nombres possibles. Dans cet exemple, il sera compris entre 0 et 99. Pour obtenir un nombre dans une plage différente, il suffit de modifier le deuxième paramètre de l'opérateur.

Une autre méthode consiste à utiliser la bibliothèque `random` introduite dans la norme C++11. Cette approche est plus recommandée car elle permet de générer des nombres de meilleure qualité et dans une plage ayant une plus grande étendue. Voici un exemple de code :

```C++
#include <random> // Inclusion de la bibliothèque random
#include <iostream>

int main() {
  // Crée un moteur de nombres aléatoires
  std::random_device rd;
  std::mt19937 gen(rd());

  // Définit une plage de nombres possibles
  std::uniform_int_distribution<int> distrib(1, 100);

  // Génère un nombre aléatoire
  int nombreAleatoire = distrib(gen); 
  std::cout << nombreAleatoire << std::endl;
  return 0;
}
```

Dans cet exemple, on utilise également la fonction `std::random_device` pour initialiser un générateur de nombres aléatoires et la classe `std::uniform_int_distribution` pour définir la plage de nombres possibles. Enfin, le nombre aléatoire est obtenu en appelant la fonction `distrib()`.

## Plongée en profondeur

Générer de vrais nombres aléatoires en informatique est en fait un défi complexe. La méthode la plus courante utilise des algorithmes mathématiques qui prennent en compte l'heure actuelle, les valeurs précédemment générées et d'autres variables pour produire une séquence de nombres qui semble aléatoire. Cependant, les ordinateurs étant des machines déterministes, ces nombres sont en fait pseudo-aléatoires, c'est-à-dire qu'ils suivent un modèle prévisible. Pour obtenir de "vrais" nombres aléatoires, les développeurs peuvent utiliser des générateurs matériels, tels que des capteurs de température ou de mouvement, qui peuvent fournir une source de données véritablement aléatoire pour alimenter les algorithmes.

## Voir aussi

- [Guide de la bibliothèque random en C++](https://en.cppreference.com/w/cpp/numeric/random)
- [Article sur la génération de nombres aléatoires en C++](https://geeksforgeeks.org/random-in-cpp/)
- [Explications détaillées sur la nature des nombres aléatoires en informatique](https://medium.com/@eternalprojects/randomness-does-not-exist-in-computers-d428ea4f3a6b)