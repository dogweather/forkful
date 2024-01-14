---
title:                "C++: Lecture des arguments en ligne de commande"
simple_title:         "Lecture des arguments en ligne de commande"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Pourquoi

Lorsque vous programmez en C++, il est souvent nécessaire de fournir à votre programme des informations à partir du terminal. Cela peut être utile pour personnaliser l'exécution du programme ou pour exécuter différentes tâches en fonction des paramètres fournis. Dans cet article, nous allons vous expliquer comment lire les arguments de ligne de commande en C++.

## Comment faire

La lecture des arguments de ligne de commande en C++ est assez simple. Tout d'abord, vous devez inclure la bibliothèque `iostream` pour pouvoir utiliser les fonctions de lecture du terminal. Ensuite, vous pouvez utiliser la fonction `main()` pour définir une liste de chaînes de caractères, qui représenteront les arguments de ligne de commande. Voici un exemple :

```C++

#include <iostream>

int main(int argc, char* argv[]) {
  // Affiche le nombre d'arguments fournis
  std::cout << "Nombre d'arguments : " << argc << std::endl;
  // Boucle pour afficher chaque argument
  for (int i = 0; i < argc; i++) {
    std::cout << "Argument " << i + 1 << " : " << argv[i] << std::endl;
  }
  return 0;
}
```

Voici un exemple d'exécution de ce programme avec différents arguments :

```
$ ./mon_programme argument1 argument2 argument3

Nombre d'arguments : 4
Argument 1 :./mon_programme
Argument 2 : argument1
Argument 3 : argument2
Argument 4 : argument3
```

Comme vous pouvez le voir, `argc` représente le nombre total d'arguments fournis, y compris le nom du programme. Et `argv` est un tableau contenant chaque argument sous forme de chaîne de caractères.

En plus de lire les arguments de la ligne de commande, vous pouvez également utiliser `getopt()` ou `getopt_long()` pour gérer les options et les paramètres d'une manière plus structurée. Nous vous conseillons de vous renseigner davantage sur ces fonctions si vous avez besoin de gérer des arguments plus complexes dans votre programme.

## Plongée en profondeur

Si vous souhaitez en savoir plus sur la lecture des arguments de ligne de commande en C++, voici quelques informations supplémentaires. Tout d'abord, sachez que le nombre maximum d'arguments possibles dépend du système d'exploitation et de l'espace disponible dans la mémoire du programme. Ensuite, il est important de noter que les arguments sont toujours renvoyés sous forme de chaînes de caractères, vous devrez donc les convertir en d'autres types de données si nécessaire.

De plus, lors de l'utilisation de `getopt()` ou `getopt_long()`, vous pouvez définir des options facultatives ou requises, et spécifier des arguments supplémentaires pour ces options. Cela peut être utile si vous avez besoin d'exécuter différentes actions en fonction de différentes combinaisons d'options et d'arguments.

## Voir aussi

Vous pouvez consulter ces liens pour en savoir plus sur la lecture des arguments de ligne de commande en C++ :

- [La documentation de `getopt()`](https://www.gnu.org/software/libc/manual/html_node/Using-Getopt.html#Using-Getopt)
- [La documentation de `getopt_long()`](https://www.gnu.org/software/libc/manual/html_node/Getopt-Long-Option-Example.html#Getopt-Long-Option-Example)
- [Un exemple plus complet de lecture d'arguments en C++](https://www.cplusplus.com/articles/DEN36Up4/)