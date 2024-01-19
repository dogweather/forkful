---
title:                "Interpolation d'une chaîne de caractères"
html_title:           "Ruby: Interpolation d'une chaîne de caractères"
simple_title:         "Interpolation d'une chaîne de caractères"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?
La "interpolation des chaînes" est une méthode pour intégrer des variables directement dans une chaîne. Les programmeurs l'utilisent pour afficher des valeurs de variables dans des messages, ce qui rend le code plus lisible et moins encombré.

## Comment faire:
```C++
#include <iostream>
#include <string>

int main() {
    std::string nom = "Pierre";
    int age = 20;

    // Création d'un message avec l'interpolation des chaînes
    std::string message = "Bonjour, " + nom + ". Vous avez " + std::to_string(age) + " ans.";

    std::cout << message << std::endl;
    
    return 0;
}
```
Sortie:
```
Bonjour, Pierre. Vous avez 20 ans.
```
## Regard approfondi
Historiquement, les programmeurs devaient utiliser des méthodes compliquées pour incorporer des variables dans les chaînes. Cependant, avec l'arrivée de l'interpolation de chaînes en C++, cette tâche est devenue beaucoup plus facile et directe.

Cependant, il est important de noter que la méthode présentée ci-dessus n'est utilisable qu'avec les chaînes et quelques types de données. Pour d'autres types, vous devrez peut-être utiliser d'autres techniques, comme les flux de chaînes (`stringstream`).

En termes de mise en œuvre, l'opérateur `+` est surchargé pour la classe `std::string` pour faciliter la concaténation de chaînes. Mais gardez à l'esprit que si vous utilisez cette méthode dans une boucle, cela pourrait affecter les performances en raison de la création de nouvelles chaînes à chaque opération.

## Voir Aussi
Pour plus d'informations, vous pouvez consulter les ressources suivantes:
- [La documentation officielle sur les chaînes en C++](http://www.cplusplus.com/reference/string/string/)
- [Un article sur l'interpolation des chaînes en C++](https://www.techiedelight.com/interpolate-strings-in-cpp/)