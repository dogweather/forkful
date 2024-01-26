---
title:                "Mettre une chaîne de caractères en majuscules"
html_title:           "C: Mettre une chaîne de caractères en majuscules"
simple_title:         "Mettre une chaîne de caractères en majuscules"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Quoi et Pourquoi ?

Capitaliser une chaîne de caractères, c’est transformer toutes ses lettres en majuscules. Les programmeurs le font pour uniformiser des textes, comme les titres ou pour faciliter les comparaisons insensibles à la casse.

## Comment faire :

```C++
#include <iostream>
#include <algorithm>
#include <string>

int main() {
    std::string s = "Bonjour, le monde!";
    std::transform(s.begin(), s.end(), s.begin(), 
        [](unsigned char c){ return std::toupper(c); });

    std::cout << s << std::endl; // Affiche "BONJOUR, LE MONDE!"
    return 0;
}
```

## Exploration Approfondie

Dans les premiers jours de l'informatique, capitaliser était important pour les systèmes qui ne supportaient que les majuscules. Aujourd'hui, bien que les systèmes soient plus souples, la capitalisation aide à éviter les soucis de comparaison de chaînes de caractères. Alternativement, au lieu de `std::transform`, on pourrait parcourir la chaîne manuellement ou utiliser des bibliothèques tierces.

Détail d'implémentation : `std::toupper` prend un `unsigned char` pour gérer correctement les caractères non ASCII. La conversion est basée sur la localisation par défaut, qui peut être changée avec `std::setlocale`.

## Voir Aussi

- Documentation de la bibliothèque standard C++ sur `std::transform` : http://www.cplusplus.com/reference/algorithm/transform/
- Documentation sur `std::toupper` : http://www.cplusplus.com/reference/cctype/toupper/
- Article sur la sensibilité à la casse dans les comparaisons de chaînes (en anglais) : https://en.cppreference.com/w/cpp/string/byte/toupper
