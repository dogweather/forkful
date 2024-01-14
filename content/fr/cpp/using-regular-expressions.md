---
title:    "C++: Utiliser les expressions régulières"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Pourquoi

Les expressions régulières sont un outil puissant pour traiter et manipuler des chaînes de caractères dans un programme C++. Ils permettent de rechercher et de remplacer des motifs spécifiques dans une grande quantité de données, ce qui peut être utile pour de nombreuses tâches telles que l'analyse de fichiers de log, la validation de formulaires ou encore la manipulation de données dans une base de données. 

## Comment faire

Pour utiliser les expressions régulières en C++, vous aurez besoin de la bibliothèque standard `<regex>` qui contient les classes et fonctions nécessaires. Voici un exemple de code pour rechercher et remplacer un motif dans une chaîne de caractères :

```C++
#include <iostream>
#include <regex>

using namespace std;

int main() {
    // Définir le motif à rechercher 
    regex pattern("le|la|les");

    // Chaîne de caractères avec le contenu à manipuler
    string str = "Le chien et la chatte se promènent dans le parc avec leurs chiots et chatons.";

    // Effectuer le remplacement avec le mot "les"
    string result = regex_replace(str, pattern, "les");

    // Afficher le résultat
    cout << result << endl;

    return 0;
}
```

Dans cet exemple, le résultat de la chaîne de caractères sera : "Les chien et les chatte se promènent dans le parc avec leurs chiots et chatons." Vous pouvez également utiliser les expressions régulières pour valider des adresses email, des numéros de téléphone ou encore pour extraire des informations spécifiques d'un texte.

## Plongée en profondeur 

Les expressions régulières en C++ utilisent la syntaxe standard pour les expressions régulières, également utilisée dans d'autres langages de programmation tels que Perl ou Python. La bibliothèque `<regex>` contient plusieurs classes qui peuvent être utilisées pour simplifier la manipulation d'expressions régulières, telles que `std::regex` pour définir le motif à rechercher, `std::smatch` pour stocker les résultats de la recherche et `std::regex_iterator` pour parcourir un texte en fonction d'un motif donné. En apprenant à utiliser ces différentes classes, vous pourrez augmenter votre efficacité et votre flexibilité dans l'utilisation des expressions régulières en C++.

## Voir aussi

- [Documentation sur les expressions régulières en C++](https://en.cppreference.com/w/cpp/regex)
- [Tutoriel sur les expressions régulières avec C++](https://www.tutorialspoint.com/cpp_standard_library/cpp_regex_library.htm)
- [Exemples pratiques d'utilisation des expressions régulières en C++](https://www.geeksforgeeks.org/regular-expressions-in-c-cpp/)