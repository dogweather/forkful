---
date: 2024-01-26 03:37:55.572557-07:00
description: "Supprimer les guillemets d'une cha\xEEne de caract\xE8res signifie retirer\
  \ ces nuisibles guillemets doubles ou simples qui encadrent notre texte (' ou \"\
  ). Les\u2026"
lastmod: '2024-03-13T22:44:58.148125-06:00'
model: gpt-4-0125-preview
summary: "Supprimer les guillemets d'une cha\xEEne de caract\xE8res signifie retirer\
  \ ces nuisibles guillemets doubles ou simples qui encadrent notre texte (' ou \"\
  ). Les\u2026"
title: "Retirer les guillemets d'une cha\xEEne"
weight: 9
---

## Quoi & Pourquoi ?
Supprimer les guillemets d'une chaîne de caractères signifie retirer ces nuisibles guillemets doubles ou simples qui encadrent notre texte (' ou "). Les programmeurs font souvent cela pour assainir les entrées, stocker du texte dans une base de données ou préparer des chaînes pour un traitement ultérieur sans l'encombrement des marques de citation.

## Comment faire :
Voici une manière directe d'éliminer ces guillemets en C++ :

```cpp
#include <iostream>
#include <algorithm>

std::string remove_quotes(std::string input) {
    input.erase(std::remove(input.begin(), input.end(), '\"'), input.end());
    input.erase(std::remove(input.begin(), input.end(), '\''), input.end());
    return input;
}

int main() {
    std::string original = R"("Bonjour, 'Monde'!")";
    std::string sans_guillemets = remove_quotes(original);
    std::cout << sans_guillemets << std::endl;
    return 0;
}
```

Exécutez cela, et vous obtiendrez :

```
Bonjour, Monde!
```

Voilà ! Les guillemets ont disparu.

## Exploration détaillée
Les guillemets sont une nuisance textuelle depuis l'aube de l'informatique. À l'époque, vous verriez des programmeurs parcourir laborieusement chaque caractère pour filtrer ces guillemets. Aujourd'hui, nous disposons de `std::remove` dans la Bibliothèque de modèles standard (STL) pour faire le gros du travail.

Des alternatives ? Bien sûr ! Vous pourriez utiliser des expressions régulières avec `std::regex` pour cibler les guillemets, mais c'est un peu comme utiliser un marteau-pilon pour casser une noix - puissant, mais peut être excessif pour des tâches simples. Pour ceux qui privilégient les versions récentes de C++, vous pourriez expérimenter avec `std::string_view` pour des approches non-modifiantes.

En termes d'implémentation, souvenez-vous que `std::remove` n'élimine pas réellement les éléments du conteneur ; il réorganise les éléments non supprimés vers l'avant et retourne un itérateur juste après la nouvelle fin de la gamme. C'est pourquoi nous avons besoin de la méthode `erase` pour couper la queue indésirable.

## Voir aussi
- Référence C++ `std::remove` : [cppreference.com](https://en.cppreference.com/w/cpp/algorithm/remove)
- Plus sur la manipulation de `std::string` : [cplusplus.com](http://www.cplusplus.com/reference/string/string/)
