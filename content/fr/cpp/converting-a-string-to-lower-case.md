---
title:                "Convertir une chaîne en minuscules"
html_title:           "Arduino: Convertir une chaîne en minuscules"
simple_title:         "Convertir une chaîne en minuscules"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# Conversion de Chaînes en Minuscules en C++: Pourquoi et Comment?

## Quoi et Pourquoi?
La conversion d'une chaîne de caractères en minuscules signifie transformer tous les caractères majuscules de cette chaîne en leurs équivalents minuscules. Les programmeurs l'utilisent pour normaliser les données, facilitant ainsi les comparaisons et recherches de chaînes.

## Comment faire:
En C++, nous avons la bibliothèque standard `<algorithm>` qui peut être utilisée pour convertir une chaîne en minuscules. Voilà comment on s'y prend:

```C++
#include<iostream>
#include<cctype>
#include<algorithm>

void toLowerCase(string &s) {
    std::transform(s.begin(), s.end(), s.begin(), 
    [](unsigned char c){ return std::tolower(c); }
    );
}

int main() {
    string s = "CONVERSION De ChaÎNE En Minuscules";
    toLowerCase(s);
    std::cout << s << std::endl;
    return 0;
}
```
Ce programme produit la sortie suivante:
```
conversion de chaîne en minuscules
```
Cela revient à parcourir chaque caractère de la chaîne et le convertir en minuscule.

## Plongée Profonde
Historiquement, la conversion de chaînes en minuscules trouve ses origines depuis le développement des premiers systèmes de bases de données. Il s'est avéré que la gestion des données sera beaucoup plus confortable et précise si toutes sont converties en un format aménageable unique.

Il existe bien sûr d'autres alternatives pour convertir une chaîne en minuscules. Par exemple, on peut boucler sur chaque caractère de la chaîne et utiliser la fonction `tolower()`. Mais, l'utilisation de la fonction `std::transform` est généralement préférée car elle est plus concise et plus optimisée.

En termes de détails d'implémentation, il convient de noter que `std::tolower` convertit uniquement les caractères ASCII. Pour des chaînes contenant des caractères non-ASCII, une approche différente pourrait être nécessaire.

## Voir Aussi
- [Documentation de `std::transform`](https://en.cppreference.com/w/cpp/algorithm/transform)
- [Discussion détaillée sur StackOverflow](https://stackoverflow.com/questions/313970/how-to-convert-stdstring-to-lower-case)
- [Article plus détaillé sur la conversion de chaînes](https://www.fluentcpp.com/2017/12/05/making-stdstring-uppercase-lowercase)