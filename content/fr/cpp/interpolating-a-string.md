---
title:                "Interpoler une chaîne de caractères"
html_title:           "C++: Interpoler une chaîne de caractères"
simple_title:         "Interpoler une chaîne de caractères"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/interpolating-a-string.md"
---

{{< edit_this_page >}}

# Qu'est-ce que c'est et pourquoi le faire?

Interpoler une chaîne de caractères, c'est insérer des variables dans une chaîne afin de la rendre dynamique. Cela permet aux programmeurs de créer du contenu personnalisé en utilisant des valeurs stockées dans des variables.

## Comment procéder:

Voici un exemple simple de l'interpolation de chaînes en C++:

\`\`\`C++
#include <iostream>
#include <string>

int main() {
    std::string nom = "John";
    std::cout << "Bonjour " << nom << "! Comment ça va?";
    return 0;
}
\`\`\`

Résultat:

\`\`\`
Bonjour John! Comment ça va?
\`\`\`

## Plongée en profondeur:

L'interpolation de chaînes est une pratique courante dans de nombreux langages de programmation. Elle a été introduite pour la première fois dans le langage de programmation Algol 68 dans les années 1960, mais a été popularisée par Perl dans les années 1990 sous le nom d'interpolation de chaîne de caractères.

Bien qu'elle soit largement utilisée, il existe d'autres techniques pour rendre les chaînes de caractères dynamiques, telles que la concaténation de chaînes de caractères et l'utilisation de modèles. Cependant, l'interpolation de chaînes reste la méthode la plus simple et la plus efficace pour de nombreux cas d'utilisation.

En C++, l'interpolation de chaînes est réalisée à l'aide de la fonction `std::cout` qui permet d'afficher du texte dans la console, ainsi que d'autres fonctions telles que `std::getline` pour récupérer des entrées d'utilisateurs.

## À voir également:

- [Documentation officielle de C++](https://isocpp.org/wiki/faq/input-output)
- [Un guide complet sur l'interpolation de chaînes en C++](https://www.learn-c.org/en/String_interpolation)
- [Une discussion sur les avantages et les inconvénients de l'interpolation de chaînes](https://stackoverflow.com/questions/3755125/what-are-all-the-advantages-and-disadvantages-of-presenting-a-string-interpolation)
# Conclusion:

En utilisant l'interpolation de chaînes, les programmeurs peuvent facilement créer du contenu dynamique et personnalisé en utilisant des variables. Cette technique est couramment utilisée et reste la méthode la plus simple pour rendre les chaînes de caractères dynamiques en C++.