---
title:                "Convertir une chaîne en minuscules"
html_title:           "C++: Convertir une chaîne en minuscules"
simple_title:         "Convertir une chaîne en minuscules"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Pourquoi

Convertir une chaîne de caractères en minuscules est une tâche courante en programmation car cela permet de normaliser les données pour faciliter les comparaisons et les recherches. De plus, de nombreuses fonctions dans les bibliothèques standard de C++ nécessitent que les chaînes soient en minuscules pour fonctionner correctement.

## Comment faire

```C++
#include <iostream>
#include <string>
#include <locale> // nécessaire pour la fonction std::tolower

int main() {
  // Déclarer et initialiser une chaîne de caractères
  std::string message = "Bonjour à tous !";

  // Boucle pour parcourir chaque caractère de la chaîne
  for (size_t i = 0; i < message.length(); i++) {
    // Utiliser la fonction std::tolower pour convertir chaque caractère en minuscule
    message[i] = std::tolower(message[i]);
  }

  // Afficher le résultat
  std::cout << message << std::endl;
  return 0;
}
```

Output: bonjour à tous !

Il est également possible d'utiliser la fonction `transform` de la bibliothèque `<algorithm>` pour effectuer la conversion en une seule ligne de code.

```C++
#include <iostream>
#include <string>
#include <algorithm> // nécessaire pour la fonction std::transform
#include <locale> // nécessaire pour la fonction std::tolower

int main() {
  // Déclarer et initialiser une chaîne de caractères
  std::string message = "Bonjour à tous !";

  // Utiliser la fonction std::transform pour convertir la chaîne en minuscules
  std::transform(message.begin(), message.end(), message.begin(),
    [](unsigned char c) { return std::tolower(c); });

  // Afficher le résultat
  std::cout << message << std::endl;
  return 0;
}
```

Output: bonjour à tous !

## Plongez plus en profondeur

La fonction `std::tolower` utilisée dans les exemples précédents renvoie un caractère en minuscule selon les règles de la locale actuelle. La locale est un ensemble de paramètres régionaux qui déterminent les règles de conversion pour différentes langues et cultures. Par défaut, la locale standard est utilisée, mais il est possible de changer la locale en utilisant la fonction `std::setlocale`.

De plus, il existe d'autres fonctions dans la bibliothèque standard de C++ pour manipuler les chaînes de caractères, telles que `std::toupper` pour convertir en majuscules et `std::stoi` pour convertir une chaîne en un entier.

## Voir aussi

- [Documentation de la fonction std::tolower sur cppreference.com](https://en.cppreference.com/w/cpp/string/byte/tolower)
- [Conversion de chaînes de caractères en C++](https://www.learncpp.com/cpp-tutorial/converting-between-string-types/)