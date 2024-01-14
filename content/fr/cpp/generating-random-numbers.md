---
title:    "C++: Génération de nombres aléatoires"
keywords: ["C++"]
---

{{< edit_this_page >}}

## Pourquoi

Ecrire un code de génération de nombres aléatoires peut sembler comme une tâche inutile, mais en réalité, cela peut être très utile dans de nombreux cas. Ce type de code est souvent utilisé dans les jeux vidéo pour créer des éléments aléatoires tels que la position des ennemis ou les butins. Il peut également être utile pour des simulations ou des tests de logiciels.

## Comment faire

Coder un générateur de nombres aléatoires en C++ est assez simple. Voici un exemple de code utilisant la fonction `rand()` pour générer 10 nombres aléatoires entre 0 et 100 :

```C++
#include <iostream>
#include <cstdlib>

int main()
{
  for(int i = 0; i < 10; i++)
  {
    std::cout << rand() % 100 << std::endl;
  }
}
```

Voici l'output de ce code :

```
47
12
80
99
65
42
34
89
23
56
```

L'utilisation de la fonction `rand()` nécessite d'inclure la librairie `cstdlib`. De plus, il est possible d'utiliser la fonction `srand()` pour initialiser la séquence de nombres aléatoires en utilisant une valeur "seed" spécifique.

Il est également possible de générer des nombres aléatoires selon une distribution précise en utilisant la librairie `random`. Voici un exemple de code qui génère 10 nombres aléatoires selon une distribution normale :

```C++
#include <iostream>
#include <random>

int main()
{
  std::random_device rd;
  std::mt19937 gen(rd());
  std::normal_distribution<> dis(0, 1);
  
  for(int i = 0; i < 10; i++)
  {
    std::cout << dis(gen) << std::endl;
  }
}
```

## Plongée en profondeur

La fonction `rand()` utilise un algorithme appelé "Linear Congruential Generator" qui génère des nombres pseudo-aléatoires. Cela signifie qu'il est possible de prédire la suite de nombres générée si l'on connait la valeur initiale de la séquence. Pour éviter cela, il est recommandé d'utiliser la fonction `srand()` avec une valeur "seed" aléatoire à chaque exécution du code.

La librairie `random` utilise des générateurs de nombres aléatoires plus complexes et offre une plus grande variété de distributions. Elle est également plus sûre à utiliser car elle ne génère pas de séquences prédictibles.

## Voir aussi

- [Documentation de la fonction rand() en C++](https://fr.cppreference.com/w/cpp/numeric/random/rand)
- [Documentation de la librairie random en C++](https://fr.cppreference.com/w/cpp/numeric/random)