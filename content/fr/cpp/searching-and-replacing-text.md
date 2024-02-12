---
title:                "Recherche et remplacement de texte"
aliases:
- fr/cpp/searching-and-replacing-text.md
date:                  2024-01-20T17:57:22.984759-07:00
model:                 gpt-4-1106-preview
simple_title:         "Recherche et remplacement de texte"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## What & Why?
La recherche et le remplacement de texte permettent de trouver des chaînes spécifiques dans un texte et de les remplacer par d'autres. Les programmeurs utilisent cette technique pour corriger des erreurs, mettre à jour des données, ou modifier du code de manière efficace et automatique.

## How to:
Le code C++ suivant montre comment rechercher et remplacer du texte avec la bibliothèque standard:

```C++
#include <iostream>
#include <string>
#include <regex>

int main() {
    std::string text = "Bonjour le monde! La programmation est amusante.";
    std::regex pattern("amusante");
    std::string newText = std::regex_replace(text, pattern, "formidable");
    
    std::cout << newText << std::endl; // Affiche "Bonjour le monde! La programmation est formidable."
    return 0;
}
```

## Deep Dive
Rechercher et remplacer du texte est une opération fondamentale dans l'édition de texte depuis des décennies. En C++, cette opération est facilitée par la bibliothèque standard, qui inclut des classes et fonctions telles que `std::string` et `std::regex`. Ces outils sont puissants et complètent les expressions régulières POSIX, utilisées avant l'intégration de la bibliothèque `<regex>` en C++11.

Il existe d'autres moyens de rechercher et remplacer du texte en C++, comme les méthodes `find` et `replace` de `std::string`, ou en utilisant des boucles et des conditions pour parcourir et modifier le texte manuellement. Cependant, le choix de l'une ou l'autre méthode dépend de la complexité du motif de recherche et du contexte d'utilisation.

Quant à l'implémentation, `<regex>` utilise l'analyse de motifs pour appliquer un modèle de recherche à un bloc de texte, permettant des remplacements conditionnels et variés basés sur des groupes capturants, des assertions et des classes de caractères.

## See Also
- [C++ Reference - RegEx](https://en.cppreference.com/w/cpp/regex)
- [C++ Documentation - Strings](http://www.cplusplus.com/reference/string/string/)
- [Regular Expressions (Regex) Tutorial](https://www.regular-expressions.info/tutorial.html)
