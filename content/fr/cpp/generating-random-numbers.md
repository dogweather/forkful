---
title:                "C++: Génération de nombres aléatoires"
programming_language: "C++"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Pourquoi

Générer des nombres aléatoires est très utile pour une variété de programmes, tels que les jeux, les simulations, et les tests de performance. Cette fonctionnalité permet de créer de l'imprévisibilité dans le fonctionnement d'un programme, ce qui le rend plus réaliste et plus intéressant.

## Comment Faire

Il existe plusieurs façons de générer des nombres aléatoires en C++. La plus simple est d'utiliser la fonction `rand()` de la bibliothèque standard `<cstdlib>`. Cette fonction renvoie un entier aléatoire entre 0 et la valeur maximale définie par la macro `RAND_MAX`. Voici un exemple de code montrant comment générer 10 nombres aléatoires et les afficher :

```C++
#include <cstdlib>
#include <iostream>

int main() {
    for (int i = 0; i < 10; i++) {
        int random = rand();
        std::cout << random << std::endl;
    }
    return 0;
}
```

Voici un exemple de sortie possible :

`1432253896` <br>
`1002384046` <br>
`1876938405` <br>
`511971850` <br>
`1110120649` <br>
`541279992` <br>
`53180817` <br>
`625328209` <br>
`781167553` <br>
`641772712`

Il est également possible de limiter la plage de nombres aléatoires en utilisant le modulo de la valeur renvoyée par `rand()`. Par exemple, pour générer des nombres aléatoires entre 0 et 9, vous pouvez utiliser `random = rand() % 10`. Voici un exemple de code utilisant cette méthode :

```C++
#include <cstdlib>
#include <iostream>

int main() {
    for (int i = 0; i < 10; i++) {
        int random = rand() % 10;
        std::cout << random << std::endl;
    }
    return 0;
}
```

Et voici un exemple de sortie possible :

`3` <br>
`6` <br>
`2` <br>
`9` <br>
`8` <br>
`5` <br>
`1` <br>
`7` <br>
`4` <br>
`0`

## Plongée Plus Profonde

Pour générer des nombres vraiment aléatoires, il est recommandé d'utiliser une distribution aléatoire telle que `uniform_int_distribution` ou `uniform_real_distribution` de la bibliothèque `<random>`. Ces distributions fournissent une meilleure aléatorité et une plus grande variété de types de données.

Il est également important de s'assurer d'initialiser correctement le générateur de nombres aléatoires en utilisant une graine différente à chaque exécution du programme. Vous pouvez utiliser `srand()` pour initialiser une graine basée sur l'heure courante du système.

## Voir aussi

Pour plus d'informations sur la génération de nombres aléatoires en C++, voici quelques ressources utiles (en anglais) :

- [Cppreference - Random library](https://en.cppreference.com/w/cpp/numeric/random)
- [GeeksforGeeks - Generating Random Numbers in C++](https://www.geeksforgeeks.org/generating-random-numbers-in-c/)
- [Cplusplus.com - rand() function](https://www.cplusplus.com/reference/cstdlib/rand/)
- [Cplusplus.com - <random> library](https://www.cplusplus.com/reference/random/)