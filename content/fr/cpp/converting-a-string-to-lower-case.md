---
title:    "C++: Convertir une chaîne en minuscules"
keywords: ["C++"]
---

{{< edit_this_page >}}

## Pourquoi

Convertissez-vous souvent des chaînes de caractères en minuscules dans vos programmes C++ ? Si oui, vous vous demandez peut-être pourquoi cette opération est importante et comment réaliser cela de manière efficace. Dans cet article, nous allons plonger dans le sujet et découvrir pourquoi la conversion en minuscules est utile dans une application et comment la réaliser en utilisant C++.

## Comment faire

La conversion d'une chaîne en minuscules peut être réalisée de différentes manières, voici quelques exemples en utilisant C++ :

```C++
#include <iostream>
#include <string>
#include <algorithm>

int main() {
    // Déclaration de notre chaîne de caractères
    std::string str = "CONVERSION EN MINUSCULES";

    // Utilisation de l'algorithme transform pour convertir chaque caractère en minuscule
    std::transform(str.begin(), str.end(), str.begin(), ::tolower);

    // Affichage de la chaîne convertie
    std::cout << str << std::endl;
    return 0;
}
```

L'exemple ci-dessus montre l'utilisation de la fonction `transform` de la bibliothèque `<algorithm>` pour convertir chaque caractère de la chaîne en minuscule. Il est également possible d'utiliser la fonction `tolower` de la bibliothèque `<string>` pour réaliser cette conversion.

```C++
#include <iostream>
#include <string>

int main() {
    // Déclaration de notre chaîne de caractères
    std::string str = "CONVERSION EN MINUSCULES";

    // Boucle pour parcourir chaque caractère et utiliser la fonction tolower pour la conversion
    for (char& c : str) {
        c = std::tolower(c);
    }

    // Affichage de la chaîne convertie
    std::cout << str << std::endl;
    return 0;
}
```

Il est également possible de réaliser cette conversion manuellement en utilisant une boucle pour parcourir chaque caractère de la chaîne et en le convertissant individuellement avec la fonction `tolower`.

Les résultats de ces deux exemples seraient les mêmes :

```
conversion en minuscules
```

## Plongeons plus en détails

La conversion en minuscules peut sembler une opération simple, mais elle est souvent nécessaire pour garantir la cohérence et la lisibilité des données dans une application. Par exemple, si vous stockez des noms d'utilisateurs dans une base de données, il est préférable que les noms soient en minuscules afin d'éviter toute confusion lors de la recherche ou du tri des données.

De plus, certaines fonctions et opérations en C++ sont sensibles à la casse, ce qui signifie qu'elles différencient les lettres majuscules et minuscules. Par conséquent, convertir une chaîne en minuscules peut éviter les erreurs lors de l'utilisation de ces fonctions.

Il est également possible d'effectuer une conversion en majuscules en utilisant la fonction `toupper` de la bibliothèque `<string>` ou l'algorithme `transform` avec la fonction `::toupper`.

Enfin, il est important de noter que la conversion d'une chaîne en minuscules peut être différente selon la langue utilisée. Par exemple, en français, la lettre "é" peut être écrite en majuscule "É" ou en minuscule "é", mais en anglais, elle est uniquement écrite en majuscule "É". Dans ce cas, une conversion en minuscules peut changer le sens des mots et causer des erreurs. Il est donc important de prendre en compte la langue lors de la conversion en minuscules.

## Voir aussi

- [Fonction `std::transform` en C++ (en anglais)](https://en.cppreference.com/w/cpp/algorithm/transform)
- [Fonction `std::tolower` de la bibliothèque `<string>` en C++ (en anglais)](https://en.cppreference.com/w/cpp/string/byte/tolower)