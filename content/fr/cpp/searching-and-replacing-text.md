---
title:                "Recherche et remplacement de texte"
html_title:           "C++: Recherche et remplacement de texte"
simple_title:         "Recherche et remplacement de texte"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# Pourquoi

La recherche et le remplacement de textes sont des opérations courantes en programmation. Cela peut être utile pour modifier rapidement de grands ensembles de données ou pour automatiser la mise à jour de certaines chaînes de caractères.

# Comment faire

Pour effectuer des recherches et des remplacements de textes en C++, nous pouvons utiliser la fonction `std::regex_replace` de la bibliothèque standard. Cette fonction prend en paramètre une expression régulière (regex) et remplace toutes les occurrences du texte correspondant dans une chaîne de caractères par un autre texte.

Voici un exemple illustrant l'utilisation de `std::regex_replace` pour remplacer toutes les occurrences du mot "vieux" par le mot "nouveau" dans une chaîne de caractères :
```C++
#include <iostream>
#include <regex>

int main()
{
    std::string texte = "Le vieux chien dort sous le vieux chêne.";
    std::regex regex("vieux");
    std::string resultat = std::regex_replace(texte, regex, "nouveau");
    std::cout << resultat << std::endl;
    // Sortie : "Le nouveau chien dort sous le nouveau chêne."
    return 0;
}
```

Nous pouvons également utiliser des expressions régulières plus complexes pour effectuer des recherches et des remplacements. Par exemple, si nous souhaitons remplacer tous les nombres dans une chaîne de caractères par leur carré, nous pouvons le faire avec l'expression régulière `"[0-9]+"` et une fonction de remplacement personnalisée :

```C++
#include <iostream>
#include <regex>
#include <cmath>

int main()
{
    std::string texte = "2 3 10 5 7";
    std::regex regex("[0-9]+");
    std::string resultat = std::regex_replace(texte, regex, [](const std::smatch& match) {
        int nombre = stoi(match.str());
        return std::to_string(nombre*nombre);
    });
    std::cout << resultat << std::endl;
    // Sortie : "4 9 100 25 49"
    return 0;
}
```

# Plongée en profondeur

La bibliothèque standard de C++ fournit une grande variété de fonctions et classes pour travailler avec les expressions régulières. En plus de `std::regex_replace`, nous avons également les fonctions `std::regex_match` et `std::regex_search` qui permettent respectivement de vérifier si une chaîne de caractères correspond à un motif donné et de chercher la première occurrence d'un motif dans une chaîne.

De plus, la bibliothèque fournit également la classe `std::regex` qui représente une expression régulière et fournit des outils pour la manipuler et la combiner avec d'autres expressions.

# Voir aussi

- [Documentation officielle de la bibliothèque standard de C++ pour les expressions régulières](https://en.cppreference.com/w/cpp/header/regex)
- [Guide complet sur les expressions régulières en C++](https://www.regular-expressions.info/cpp.html)