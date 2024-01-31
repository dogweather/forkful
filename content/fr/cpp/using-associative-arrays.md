---
title:                "Utilisation des tableaux associatifs"
date:                  2024-01-30T19:10:13.505463-07:00
model:                 gpt-4-0125-preview
simple_title:         "Utilisation des tableaux associatifs"

category:             "C++"
tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/using-associative-arrays.md"
changelog:
  - 2024-01-30, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Quoi et pourquoi ?

Les tableaux associatifs, connus sous le nom de `std::map` ou `std::unordered_map` en C++, comblent le fossé entre les indices de tableau et les données du monde réel, vous permettant d'utiliser des clés significatives. Ils sont la solution privilégiée lorsque vous avez besoin de recherches, d'insertions et de suppressions rapides en utilisant des clés plutôt que des positions d'index.

## Comment faire :

En C++, les tableaux associatifs prennent vie avec les en-têtes `<map>` et `<unordered_map>`. Voyons des exemples pour observer les deux en action.

### Utilisation de `std::map`

`std::map` maintient les éléments triés en fonction de la clé. Voici comment vous commencez :

```C++
#include <iostream>
#include <map>
#include <string>

int main() {
    std::map<std::string, int> ageMap;
    
    // Insertion de valeurs
    ageMap["Alice"] = 30;
    ageMap["Bob"] = 25;
    
    // Accès aux valeurs
    std::cout << "Âge de Bob : " << ageMap["Bob"] << std::endl;
    
    // Itération sur une map
    for(const auto &pair : ageMap) {
        std::cout << pair.first << " a " << pair.second << " ans." << std::endl;
    }
    
    return 0;
}
```

### Utilisation de `std::unordered_map`

Quand l'ordre n'a pas d'importance, mais que la performance compte, `std::unordered_map` est votre allié, offrant une complexité moyenne plus rapide pour les insertions, les recherches et les suppressions.

```C++
#include <iostream>
#include <unordered_map>
#include <string>

int main() {
    std::unordered_map<std::string, double> productPrice;
    
    // Insertion de valeurs
    productPrice["milk"] = 2.99;
    productPrice["bread"] = 1.99;
    
    // Accès aux valeurs
    std::cout << "Prix du lait : $" << productPrice["milk"] << std::endl;
    
    // Itération sur un unordered_map
    for(const auto &pair : productPrice) {
        std::cout << pair.first << " coûte $" << pair.second << std::endl;
    }
    
    return 0;
}
```

## Plongée profonde

Les tableaux associatifs en C++, en particulier `std::map` et `std::unordered_map`, ne concernent pas seulement le stockage d'éléments. Ils fournissent une base pour une gestion de données plus complexe en permettant des opérations comme la recherche, l'insertion et la suppression dans des complexités temporelles efficaces (logarithmique pour `std::map` et en temps moyen constant pour `std::unordered_map`). Cette efficacité provient des structures de données sous-jacentes : un arbre équilibré pour `std::map` et une table de hachage pour `std::unordered_map`.

Historiquement, avant qu'ils ne fassent partie de la bibliothèque standard, les programmeurs devaient implémenter leurs propres versions ou utiliser des bibliothèques tierces, conduisant à des incohérences et des inefficacités potentielles. L'inclusion des maps dans la bibliothèque standard du C++ a non seulement standardisé leur utilisation mais aussi optimisé leur performance à travers différents compilateurs et plateformes.

Bien que les deux soient puissants, le choix entre un `std::map` et un `std::unordered_map` repose sur les spécificités de votre cas d'utilisation. Besoin de données ordonnées et cela ne vous dérange pas une légère perte de performance ? Optez pour `std::map`. Si vous recherchez la vitesse et que l'ordre n'a pas d'importance, `std::unordered_map` est probablement votre meilleur choix.

Cependant, il est important de noter que lorsqu'on travaille avec des structures de données complexes, il y a toujours des compromis. Dans certains cas de niche, d'autres structures de données ou même des bibliothèques tierces pourraient offrir une meilleure performance ou des fonctionnalités adaptées à vos besoins particuliers. Pesez toujours vos options en fonction des exigences de votre projet.
