---
title:                "C++: Utiliser des expressions régulières"
simple_title:         "Utiliser des expressions régulières"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Pourquoi

Les expressions régulières sont une partie essentielle de la programmation en C++. Elles sont utilisées pour la recherche et la manipulation de chaînes de caractères avec une grande précision et efficacité. Si vous êtes un programmeur, les expressions régulières peuvent simplifier et accélérer considérablement votre travail avec les chaînes de caractères.

## Comment Faire

Voici comment utiliser les expressions régulières en C++ :

```C++
#include <iostream>
#include <regex>

using namespace std;

int main() {
    // Définition d'une expression régulière pour une adresse e-mail valide
    regex pattern("\\b[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,4}\\b");

    // Chaîne de caractères à tester
    string email = "bonjour123@gmail.com";

    // Vérification si la chaîne correspond au modèle défini
    if (regex_match(email, pattern)) {
        cout << "Adresse e-mail valide !" << endl;
    } else {
        cout << "Adresse e-mail invalide." << endl;
    }

    return 0;
}
```

La sortie de ce code sera "Adresse e-mail valide !". Vous pouvez également utiliser des expressions régulières pour la recherche et le remplacement de texte dans des chaînes de caractères.

## Plongée Profonde

Les expressions régulières sont un ensemble de règles pour décrire un modèle de chaîne de caractères. Ces règles peuvent inclure des caractères spéciaux et des opérateurs pour rendre la recherche plus précise. Il existe également des bibliothèques de fonctions spéciales pour les expressions régulières, comme "regex_match" qui vérifie si la chaîne donnée correspond au modèle défini.

Il est important de noter que les expressions régulières sont sensibles à la casse, c'est-à-dire qu'elles font la différence entre les majuscules et les minuscules. Il est également possible d'utiliser des drapeaux pour rendre la recherche insensible à la casse ou pour effectuer des recherches dans des chaînes de caractères multilignes.

Il existe de nombreuses ressources en ligne pour vous aider à apprendre et à maîtriser les expressions régulières en C++. Il est également utile de pratiquer et d'expérimenter avec différents modèles pour mieux comprendre leur fonctionnement.

## Voir Aussi

- [Documentation sur les expressions régulières en C++](https://fr.cppreference.com/w/cpp/regex)
- [Tutoriel sur les expressions régulières en C++](https://www.tutorialspoint.com/cpp_standard_library/cpp_regex_library.htm)
- [Exemples sur l'utilisation des expressions régulières en C++](https://github.com/learn-with-leap/Examples/tree/master/C%2B%2B/Regex)