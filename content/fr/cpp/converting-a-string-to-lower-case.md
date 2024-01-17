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

## Qu'est-ce que c'est et pourquoi le faire?

La conversion d'une chaîne de caractères en minuscules est une manipulation courante pour les programmeurs. Elle consiste à convertir tous les caractères en lettres minuscules afin de standardiser la chaîne de caractères pour une utilisation ultérieure. Les programmeurs ont souvent besoin de manipuler des chaînes de caractères, et la conversion en minuscules peut faciliter le traitement et la comparaison des données.

## Comment faire:

```C++
#include <iostream>
#include <algorithm>
#include <string>

int main() {
  // Exemple de chaîne de caractères en majuscules
  std::string str = "PROGRAMMATION C++";
    
  // Conversion de la chaîne en minuscules
  std::transform(str.begin(), str.end(), str.begin(), ::tolower);
    
  // Affichage du résultat
  std::cout << str << '\n';
  // Output: programmation c++
  return 0;
}
```

## Profonde plongée:

La conversion de chaînes de caractères en minuscules remonte aux débuts de la programmation informatique et est couramment utilisée dans de nombreux langages de programmation. Dans certains langages, comme Python, la conversion de chaînes de caractères en minuscules peut se faire simplement en utilisant une méthode intégrée. Cependant, en C++, la méthode la plus courante consiste à utiliser la fonction ```transform ()```, qui nécessite l'utilisation d'un itérateur et de la fonction ```tolower ()```. Il existe également des bibliothèques tierces qui offrent des fonctions spécifiques pour la conversion de chaînes en minuscules.

## Voir aussi:

- [Documentation](https://en.cppreference.com/w/cpp/string/byte/tolower)
- [Article sur la manipulation de chaînes de caractères en C++](https://www.geeksforgeeks.org/string-manipulation-in-c-2/)
- [Utilisation de bibliothèques tierces pour la manipulation de chaînes de caractères en C++](https://www.boost.org/doc/libs/1_62_0/doc/html/string_algo.html)