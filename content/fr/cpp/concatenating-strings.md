---
title:    "C++: Concaténation de chaînes de caractères"
keywords: ["C++"]
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes nouveau dans le monde de la programmation en C++, vous avez probablement entendu parler de la concaténation de chaînes de caractères. Mais pourquoi est-ce important? La concaténation de chaînes de caractères est un concept fondamental dans de nombreux projets de programmation, car elle permet de combiner plusieurs chaînes de caractères ensemble pour créer une chaîne plus longue et plus complexe. Cela peut être utile pour afficher des messages d'erreur, construire des URL dynamiques ou organiser des données pour le traitement ultérieur.

## Comment faire

La concaténation de chaînes de caractères peut être réalisée en utilisant l'opérateur "+" pour ajouter des chaînes de caractères ensemble. Par exemple:

```C++
#include <iostream>

int main() {
    std::string nom = "Marie";
    std::string prenom = "Dupont";
    std::cout << nom + " " + prenom << std::endl;
    return 0;
}
```
Output: Marie Dupont

Comme vous pouvez le voir, en utilisant l'opérateur "+", nous avons combiné les chaînes "nom" et "prenom" pour créer la nouvelle chaîne "Marie Dupont". Il est important de noter que le type de données utilisé doit être "string" et non "char" pour un résultat correct.

## Profondeur

En plus de l'opérateur "+", il existe également une fonction "concat" dans la bibliothèque standard de C++ qui permet de concaténer des chaînes de caractères. Cette fonction peut être plus efficace que l'opérateur "+" dans certains cas, car elle utilise une méthode différente pour combiner les chaînes.

Il est également important de comprendre l'allocation de mémoire lors de l'utilisation de la concaténation de chaînes de caractères. Comme les chaînes de caractères sont immuables en C++, vous devez gérer manuellement la mémoire allouée pour chaque nouvelle chaîne créée pendant le processus de concaténation. Sinon, cela pourrait entraîner des fuites de mémoire et affecter les performances de votre programme.

## Voir aussi

- [C++ Concatenation of Strings](https://www.programiz.com/cpp-programming/string-concatenation)
- [How to Concatenate Strings in C++](https://www.geeksforgeeks.org/how-to-concatenate-strings-in-c/?ref=lbp)
- [C++ String Concatenation Benchmark](https://stackoverflow.com/questions/17690898/c-string-concatenation-benchmark?rq=1)