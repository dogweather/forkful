---
title:                "Utiliser les expressions régulières"
html_title:           "C: Utiliser les expressions régulières"
simple_title:         "Utiliser les expressions régulières"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/using-regular-expressions.md"
---

{{< edit_this_page >}}

# Régulier Expressions en C++ : Un Guide Simplifié 

## Qu'est-ce que c'est  & Pourquoi ?

Les expressions régulières sont un outil puissant pour la manipulation de chaînes. Les programmeurs les utilisent pour rechercher, remplacer, et valider les chaînes de caractères.

## Comment faire :

Observez l'exemple suivant. Nous allons vérifier si une chaîne de caractères contient une certaine séquence de lettres.

```C++
#include <regex>
#include <string>
#include <iostream>

int main() {
    std::string s = "C++11 et ses expressions régulières";

    // Je cherche le mot "C++11" p. ex.
    std::regex e ("C\\+\\+11");

    // Testons si ça matche
    if (std::regex_search(s,e))
        std::cout << "La sequence 'C++11' a été trouvée!" << std::endl;

    return 0;
}
```
Ce qui produit :

```shell
La sequence 'C++11' a été trouvée!
```

## Plus de Détails :

1. **Contexte historique** : Le support des expressions régulières a été ajouté à C++ dans la version C++11, après des décennies d'existence dans d'autres langages comme PERL et Java.
2. **Alternatives** : Vous pouvez également utiliser des bibliothèques tierces telles que Boost.Regex qui offre une prise en charge étendue des expressions régulières.
3. **Détails d'implémentation** : L'execution des expressions régulières en C++ dépend de la STL (Standard Template Library) où la classe regex est définie dans l'en-tête `<regex>`.

## Voir Aussi :

Consultez les ressources supplémentaires ci-dessous :

1. [C++ Reference: regex](http://en.cppreference.com/w/cpp/regex) - Documentation officielle sur les expressions régulières en C++.
2. [Boost Libraries](http://www.boost.org/doc/libs/1_64_0/libs/regex/doc/html/index.html) - Documentation Boost.Regex.
3. [Using Regular expressions in C++](http://www.cplusplus.com/reference/regex/) - Un tutoriel détaillé sur les expressions régulières en C++.

Le succès en programmation nécessite de la pratique. Alors, pratiquez-vous avec les expressions régulières en C++!