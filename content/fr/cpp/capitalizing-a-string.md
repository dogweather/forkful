---
title:                "Mettre en majuscule une chaîne"
html_title:           "C++: Mettre en majuscule une chaîne"
simple_title:         "Mettre en majuscule une chaîne"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

--------

## Qu'est-ce que c'est & Pourquoi ? 
Capitaliser une chaîne de caractères revient à convertir tous les caractères alphabétiques d'une chaîne en majuscules. Les programmeurs le font souvent pour normaliser ou standardiser les données textuelles, ce qui facilite les comparaisons et les recherches.

## Comment faire :
Voici un exemple de code en C++ pour capitaliser une chaîne de caractères:

```C++
#include<iostream>
#include<algorithm>
#include<string>

int main(){
    std::string str = "programmation en c++";
    std::transform(str.begin(), str.end(), str.begin(), ::toupper);
    std::cout<<str<<std::endl;

    return 0;
}
```

Et voici ce que ça donne en sortie:

```C++
PROGRAMMATION EN C++
```

## Plongée en profondeur
Historiquement, la capitalisation des chaînes de caractères est largement utilisée dans des contextes où l'uniformité est requise, comme la saisie de données, le traitement de texte et l'analyse syntaxique. En C++, on capitalise généralement une chaîne en utilisant la fonction `transform` de la bibliothèque algorithmique, combinée à la fonction `toupper` de la bibliothèque cctype.

Il existe d'autres moyens de capitaliser une chaîne, par exemple en passant par le caractère ASCII pour chaque caractère de la chaîne. Cependant, l'approche mentionnée ci-dessus est généralement plus sûre et plus intuitive.

En termes de détails d'implémentation, la fonction `toupper` convertit chaque caractère minuscule en majuscule. Si le caractère n'est pas une minuscule alphabétique, il est retourné sans modification.

## Voir également
- [std::transform](https://en.cppreference.com/w/cpp/algorithm/transform) pour une explication plus approfondie sur `std::transform`.
- [std::toupper](http://www.cplusplus.com/reference/cctype/toupper/) pour une connaissance complète de `std::toupper`.
- [C++ String manipulation](https://www.learn-cpp.org/en/String_Manipulation) pour plus de conseils et astuces sur la manipulation des chaînes de caractères en C++.