---
title:                "Supprimer les caractères correspondant à un motif"
html_title:           "C++: Supprimer les caractères correspondant à un motif"
simple_title:         "Supprimer les caractères correspondant à un motif"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# Quoi & Pourquoi? 
Supprimer des caractères correspondant à un motif est une technique utilisée par les programmeurs pour supprimer des parties spécifiques d'une chaîne de caractères selon un motif donné. Cette technique est souvent utilisée pour nettoyer les données en éliminant les caractères indésirables ou pour filtrer certains types de données d'un ensemble plus large.

# Comment faire:
Voici un exemple simple de code en C++ pour supprimer tous les espaces d'une chaîne de caractères:
```
#include <iostream>
#include <string>

using namespace std;

int main() {
  string str = "Bonjour tout le monde!";

  // Supprimer les espaces de la chaîne
  str.erase(remove(str.begin(), str.end(), ' '), str.end());

  cout << str; // "Bonjourtoutlemonde!" sera affiché

  return 0;
}
```
Si vous souhaitez supprimer des caractères spécifiques d'une chaîne de caractères en fonction d'un motif, vous pouvez utiliser des expressions régulières. Voici un exemple de code utilisant la bibliothèque <regex>:
```
#include <iostream>
#include <regex>

using namespace std;

int main() {
  string str = "123abc456def789ghi";

  // Supprimer toutes les lettres de la chaîne
  str = regex_replace(str, regex("[a-zA-Z]"), "");

  cout << str; // "123456789" sera affiché

  return 0;
}
```

# Plongée en profondeur:
Supprimer des caractères correspondant à un motif a été rendu possible grâce à l'utilisation de la bibliothèque <algorithm> en C++. Cette bibliothèque contient une fonction appelée "remove" qui renvoie un nouvel itérateur après avoir "supprimé" les éléments correspondants à un motif donné. Il est également possible d'effectuer cette tâche à l'aide d'une boucle for, mais cela peut être plus long et plus sujet aux erreurs.

Il existe également d'autres façons de nettoyer et de filtrer les données, telles que l'utilisation de fonctions telles que "find" et "substr" pour extraire des parties spécifiques d'une chaîne de caractères. Cependant, cela peut être plus compliqué et moins efficace que l'utilisation de la fonction "remove".

# Voir aussi:
- [Documentation officielle de la bibliothèque <algorithm> en C++](https://en.cppreference.com/w/cpp/algorithm)
- [Documentation officielle de la bibliothèque <regex> en C++](https://en.cppreference.com/w/cpp/regex)