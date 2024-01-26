---
title:                "Concaténation de chaînes de caractères"
date:                  2024-01-20T17:34:08.315368-07:00
model:                 gpt-4-1106-preview
simple_title:         "Concaténation de chaînes de caractères"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/concatenating-strings.md"
---

{{< edit_this_page >}}

## What & Why?
La concaténation des chaînes, c'est quand on fusionne deux textes en un seul. Les développeurs la font pour construire des messages dynamiques, gérer des chemins de fichiers, ou tout simplement, pour agencer des données textuelles.

## How to:
```C++
#include <iostream>
#include <string>

int main() {
    std::string bonjour = "Bonjour";
    std::string monde = " monde!";
    std::string salutation = bonjour + monde; // Concaténation

    std::cout << salutation << std::endl; // Affiche "Bonjour monde!"
    
    // Concaténer avec 'append' (également possible)
    std::string question = "Comment ça ";
    question.append("va ?");

    std::cout << question << std::endl; // Affiche "Comment ça va ?"

    return 0;
}
```

## Deep Dive:
Concaténer des chaînes de caractères est aussi vieux que la programmation elle-même. Dans le passé, les langages comme le C utilisaient des fonctions comme `strcat` pour cette tâche, mais cela pouvait causer des problèmes de sécurité tels que les débordements de tampon.

Avec le C++, vous pouvez utiliser l'opérateur `+` ou la méthode `append` pour réaliser cette opération de manière sûre avec la classe `std::string`. L'opérateur `+` est simple et pratique, tandis que `append` est un peu plus flexible : il permet de concaténer directement des caractères, des parties d'autres chaînes, ou même des répétitions de caractères particuliers.

## See Also:
- Le tutoriel C++ de cplusplus.com sur les chaînes : http://www.cplusplus.com/reference/string/string/
- Documentation de cppreference sur la classe std::string : https://en.cppreference.com/w/cpp/string/basic_string
- Article Wikipédia sur la concaténation des chaînes de caractères : https://fr.wikipedia.org/wiki/Concat%C3%A9nation
