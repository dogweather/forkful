---
title:                "C++: Suppression de caractères correspondant à un motif"
simple_title:         "Suppression de caractères correspondant à un motif"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous travaillez avec du texte dans un programme C++, il est probable que vous ayez besoin de supprimer certains caractères qui correspondent à un motif spécifique. Que vous souhaitiez nettoyer les données ou effectuer des modifications dans une chaîne de caractères, savoir comment supprimer des caractères correspondants peut être très utile.

## Comment le faire

Pour supprimer des caractères correspondants à un motif donné en C++, vous pouvez utiliser la fonction de bibliothèque standard `remove_if`. Cette fonction prend deux arguments: un itérateur de début et un itérateur de fin, qui délimitent la plage de la chaîne de caractères que vous souhaitez modifier. Dans le même temps, vous devez également fournir une fonction prédicat en tant que troisième argument, qui spécifie les caractères que vous souhaitez supprimer.

Voici un exemple de code montrant comment utiliser la fonction `remove_if` pour supprimer tous les caractères numériques d'une chaîne de caractères:

```C++
#include <iostream>
#include <string>
#include <algorithm>

using namespace std;

int main() {
    string str = "1a2b3c4d";
    str.erase(remove_if(str.begin(), 
                         str.end(), 
                         [](char c) { return isdigit(c); }), 
                         str.end());
    cout << str << endl;
    return 0;
}
```

La sortie de ce code sera `abcd`, la chaîne de caractères finale sans les caractères numériques.

## Plongée en profondeur

S'il y a plusieurs occurrences du motif dans la chaîne de caractères, `remove_if` ne supprimera que le premier et laissera les autres. Si vous souhaitez supprimer toutes les occurrences, vous pouvez utiliser la fonction `std::erase`. Voici un exemple:

```C++
#include <iostream>
#include <string>

using namespace std;

int main() {
    string str = "apple, banana, cherry";
    const string pattern = ", ";
    while (str.find(pattern) != string::npos) {
        str.erase(str.find(pattern), pattern.size());
    }
    cout << str << endl;
    return 0;
}
```

La sortie de ce code sera `applebanana, cherry`, la chaîne de caractères finale sans la virgule et l'espace.

## Voir aussi

- [La documentation de remove_if en C++](https://en.cppreference.com/w/cpp/algorithm/remove)
- [Le tutoriel sur les itérateurs en C++](https://www.cplusplus.com/doc/tutorial/iterators/)