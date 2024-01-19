---
title:                "Extraction de sous-chaînes"
html_title:           "Arduino: Extraction de sous-chaînes"
simple_title:         "Extraction de sous-chaînes"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/extracting-substrings.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?
Extraire des sous-chaînes c'est récupérer une certaine partie d’une chaîne de caractères. Les programmeurs le font pour traiter des données à petite échelle, comme analyser une adresse e-mail ou un fichier de journal.

## Comment faire:
Utilisons la fonction `substr()` de la bibliothèque standard de C++. Voir l'exemple en dessous:

```C++
#include <iostream> 
#include <string> 

int main() 
{ 
    std::string s = "Programmation C++"; 

    std::string s1 = s.substr(0, 13); 
    std::cout << s1 << '\n'; 
    // Résultat : Programmation 

    std::string s2 = s.substr(14, 17); 
    std::cout << s2 << '\n';
    // Résultat : C++
} 
```

## Plongée en profondeur:
La méthode `substr()` a été introduite dans le standard C++98. Il existe des alternatives pour extraire des sous-chaînes. La bibliothèque Boost propose également des méthodes pour extraire des sous-chaînes. Quant à l'implémentation, `substr()` utilise l'algorithme de copie pour construire la nouvelle chaîne.

## Voir aussi:
Pour plus d'informations, visitez ces ressources liées:

1. [Documentation officielle de substr()](http://www.cplusplus.com/reference/string/string/substr/)
2. [Bibliothèque Boost C++](https://www.boost.org/doc/libs/)
3. [C++ Standard Library](http://www.cplusplus.com/reference/string/string/)