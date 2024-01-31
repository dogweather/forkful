---
title:                "Extraction de sous-chaînes"
date:                  2024-01-20T17:45:20.998984-07:00
model:                 gpt-4-1106-preview
simple_title:         "Extraction de sous-chaînes"

category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why? (Quoi & Pourquoi ?)

Extraire des sous-chaînes, c'est prendre une partie spécifique d'une chaîne de caractères. On fait ça pour analyser, manipuler, ou valider des données textuelles – comme couper une baguette pour ne prendre que le morceau qu'on adore.

## How to: (Comment faire :)

```C++
#include <iostream>
#include <string>

int main() {
    std::string text = "Bonjour, je suis une sous-chaîne!";
    
    // Extraire les sous-chaînes en utilisant substr
    std::string salutation = text.substr(0, 7); // "Bonjour"
    std::string phrase = text.substr(9, 14); // "je suis une s"
    
    std::cout << salutation << std::endl; // Affiche: Bonjour
    std::cout << phrase << std::endl; // Affiche: je suis une s
    
    // Utilisation de find pour localiser une sous-chaîne avant extraction
    size_t pos = text.find("suis");
    std::string mots = (pos != std::string::npos) ? text.substr(pos) : "Pas trouvé";
    
    std::cout << mots << std::endl; // Affiche: suis une sous-chaîne!
    
    return 0;
}
```

## Deep Dive (Plongée en profondeur)

Historiquement, l'extraction de sous-chaînes est une brique fondamentale de la manipulation de texte, présente dès les premiers langages de programmation. En C++, on utilise souvent la méthode `substr` de la classe `std::string`. Elle est claire et directe : `substr(pos, len)` où `pos` est la position de début et `len` est la longueur de la sous-chaîne.

Des alternatives existent, incluant les méthodes `find` pour localiser une sous-chaîne avant de l'extraire et `erase` ou `replace` pour modifier la chaîne originale. Les expressions régulières (`<regex>`) sont un outil puissant pour extraire des sous-chaînes complexes mais sont plus lourdes en termes de performance.

En C++ moderne, l'extraction de sous-chaînes est bien optimisée et les performances ne sont généralement pas un souci pour des opérations typiques. Néanmoins, pour des manipulations massives de chaînes, les détails d'implémentation, comme la copie de données ou l'utilisation de la mémoire, sont à considérer attentivement.

## See Also (Voir Aussi)

- [cplusplus.com - std::string::substr](http://www.cplusplus.com/reference/string/string/substr/)
- [cppreference.com - String Operations](https://en.cppreference.com/w/cpp/string/basic_string)
- [cplusplus.com - Regex library](http://www.cplusplus.com/reference/regex/)
