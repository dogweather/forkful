---
title:    "C++: Génération de nombres aléatoires"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Pourquoi

Les nombres aléatoires sont essentiels dans la programmation, car ils permettent aux développeurs de créer des applications plus dynamiques et interactives. En utilisant des nombres aléatoires, les programmes peuvent générer des résultats variés et surprenants, offrant ainsi une expérience unique à chaque exécution.

# Comment faire

Pour générer des nombres aléatoires en C++, vous pouvez utiliser la fonction `rand()` de la bibliothèque standard `cstdlib`. Cette fonction renvoie un nombre entier pseudo-aléatoire compris entre 0 et la valeur maximum définie dans la macro `RAND_MAX`. Voici un exemple de code pour générer un nombre aléatoire entre 1 et 100 :

```C++
#include <cstdlib>
#include <iostream>

int main() {
    int rand_num = (rand() % 100) + 1;
    std::cout << "Le nombre aléatoire est : " << rand_num << std::endl;
    return 0;
}
```

L'exécution de ce code peut produire des résultats tels que : "Le nombre aléatoire est : 42" ou "Le nombre aléatoire est : 87". Vous pouvez également utiliser la fonction `srand()` pour initialiser le générateur de nombres aléatoires avec une graine différente à chaque exécution, ce qui garantit une plus grande variété dans les résultats.

# Plongée en profondeur

Il est important de noter que les nombres générés par la fonction `rand()` ne sont pas complètement aléatoires, mais plutôt pseudo-aléatoires car ils sont basés sur une algorithme déterministe. Cela signifie que les mêmes nombres seront générés dans le même ordre à chaque exécution du programme, sauf si la graine est modifiée.

Pour obtenir une plus grande aléatoire dans vos nombres, vous pouvez utiliser les fonctions `mt19937` et `uniform_int_distribution` de la bibliothèque `<random>`. Voici un exemple de code pour générer un nombre aléatoire entre 1 et 100 grâce à une distribution uniforme :

```C++
#include <random>
#include <iostream>

int main() {
    std::random_device rd;
    std::mt19937 gen(rd());
    std::uniform_int_distribution<> dist(1, 100);
    int rand_num = dist(gen);
    std::cout << "Le nombre aléatoire est : " << rand_num << std::endl;
    return 0;
}
```

# Voir aussi

- [Documentation sur la fonction `rand()`](https://www.cplusplus.com/reference/cstdlib/rand/)
- [Documentation sur la fonction `srand()`](https://www.cplusplus.com/reference/cstdlib/srand/)
- [Documentation sur `<random>`](https://www.cplusplus.com/reference/random/)