---
date: 2024-01-20 17:37:51.443419-07:00
description: "How to (Comment faire) : En C++, vous pouvez transformer une cha\xEE\
  ne en minuscules en utilisant la biblioth\xE8que standard ."
lastmod: '2024-03-13T22:44:58.146902-06:00'
model: gpt-4-1106-preview
summary: "En C++, vous pouvez transformer une cha\xEEne en minuscules en utilisant\
  \ la biblioth\xE8que standard."
title: "Conversion d'une cha\xEEne de caract\xE8res en minuscules"
weight: 4
---

## How to (Comment faire) :
En C++, vous pouvez transformer une chaîne en minuscules en utilisant la bibliothèque standard :

```C++
#include <iostream>
#include <string>
#include <algorithm>

std::string toLowerCase(const std::string& input) {
    std::string output = input;
    std::transform(output.begin(), output.end(), output.begin(), 
                   [](unsigned char c){ return std::tolower(c); });
    return output;
}

int main() {
    std::string text = "Bonjour, PROGRAMMEUR!";
    std::string lowerCaseText = toLowerCase(text);
    
    std::cout << lowerCaseText << std::endl; // "bonjour, programmeur!"
    return 0;
}
```

## Deep Dive (Plongée en profondeur) :
Historiquement, on pouvait transformer les majuscules en minuscules à la main, en parcourant une chaîne et en utilisant la table ASCII pour ajuster les valeurs des lettres majuscules. Aujourd'hui, on utilise `std::tolower`, qui est plus robuste et gère les caractères non-ASCII. Attention, cette fonction nécessite le paramètre de type `unsigned char` pour éviter des comportements inattendus avec `char` signé.

Il existe d'autres méthodes, telles que `boost::algorithm::to_lower` de la bibliothèque Boost, qui peut offrir plus de fonctionnalités.

En interne, `std::tolower` travaille avec la localisation courante, ce qui permet de gérer les cas spécifiques de certaines langues en termes de casse.

## See Also (Voir également) :
- Documentation de `std::transform`: https://en.cppreference.com/w/cpp/algorithm/transform
- Documentation de `std::tolower`: https://en.cppreference.com/w/cpp/string/byte/tolower
- Bibliothèque Boost: https://www.boost.org/
- Guide sur la localisation en C++: https://en.cppreference.com/w/cpp/locale/locale
