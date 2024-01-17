---
title:                "Génération de nombres aléatoires"
html_title:           "C++: Génération de nombres aléatoires"
simple_title:         "Génération de nombres aléatoires"
programming_language: "C++"
category:             "C++"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Génération de Nombres Aléatoires en C++
## Qu'est-ce Que C'est Et Pourquoi le Faire?
La génération de nombres aléatoires est le processus de création de valeurs aléatoires dans un programme informatique. Les programmeurs utilisent cette technique pour apporter une certaine variabilité dans leurs applications, ou pour simuler des situations aléatoires telles que des jeux de hasard.

## Comment Faire:
```C++
#include <iostream>
#include <cstdlib> // Permet d'utiliser la fonction rand()

int main() {
   int randomNumber = rand(); // Génère un nombre aléatoire
   std::cout << "Voici un nombre aléatoire : " << randomNumber << std::endl;
   return 0;
}
```
Output:
```
Voici un nombre aléatoire : [un nombre aléatoire différent à chaque exécution]
```

Il est également possible de générer des nombres aléatoires dans une certaine plage en utilisant des opérations mathématiques sur la valeur retournée par `rand()`. Par exemple, si l'on souhaite générer un nombre aléatoire entre 1 et 10:
```C++
int randomNumber = 1 + rand() % 10;
```
## Plongée Profonde:
La génération de nombres aléatoires est une technique largement utilisée en informatique, particulièrement pour les jeux et les simulations. Cependant, la génération de nombres totalement aléatoires est un défi complexe en informatique, puisque les ordinateurs utilisent des algorithmes déterministes. Pour cette raison, la plupart des programmes informatiques utilisent des générateurs de nombres pseudo-aléatoires, qui produisent des séries de nombres apparemment aléatoires à partir d'une valeur de départ appelée "seed".

Il existe également des libraries spécialisées pour la génération de nombres aléatoires, telles que <random> en C++11 qui fournit des générateurs plus performants et des distributions plus précises.

## Voir Aussi:
- [Documentation officielle de rand() en C++](https://www.cplusplus.com/reference/cstdlib/rand/)
- [Documentation officielle de <random> en C++](https://www.cplusplus.com/reference/random/)
- [Générateur de nombres aléatoires en ligne](https://www.random.org/integer-sets/)