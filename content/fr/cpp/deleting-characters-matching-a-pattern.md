---
title:                "Suppression de caractères correspondant à un motif"
html_title:           "C: Suppression de caractères correspondant à un motif"
simple_title:         "Suppression de caractères correspondant à un motif"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

---
## Quoi & Pourquoi ?

La suppression de caractères selon un motif est le processus d'identification et d'élimination de caractères spécifiques d'une chaîne. Les développeurs le font pour nettoyer les données, manipuler les chaînes et obtenir des informations précises.

## Comment faire :

Pour illustrer, voici comment supprimer toutes les occurrences de certains caractères d'une chaîne en C++ :

```C++
#include <algorithm>
#include <string>

int main() {
    std::string str = "abacabadabacaba";
    char c = 'a';
    str.erase(std::remove(str.begin(), str.end(), c), str.end());
}
```

L'exemple ci-dessus supprime toutes les incidences de 'a' dans la chaîne.

## Plongée Profonde

Historiquement, la suppression de caractères d'une chaîne a été un aspect crucial de la manipulation de texte, avec des applications allant de l'édition de texte à la préparation des données pour l'apprentissage automatique.

Une alternative courante à l'utilisation de `std::remove` est de créer une nouvelle chaîne qui ne contient que les caractères que nous avons l'intention de garder.

```C++
std::string str = "abacabadabacaba";
char c = 'a';
std::string new_str;
for (char& ch : str) {
    if (ch != c) {
        new_str += ch;
    }
}
```

Notez que `std::remove` ne réduit pas la taille de la chaîne, il modifie plutôt la séquence en déplaçant les éléments qui ne sont pas à supprimer vers le début, puis renvoie un itérateur pointant juste après le dernier élément valide. Pour réduire la taille de la chaîne, nous devons appeler explicitement `std::string::erase`.

## Voir Aussi

Pour en apprendre davantage sur le traitement de texte en C++, visitez ces sites :

- Manipulation de chaînes en C++ chez [cplusplus.com](http://www.cplusplus.com/cplusplus-strings/)
- Documentation officielle C++ de [`std::remove` et `std::string::erase`](https://en.cppreference.com/w/cpp/algorithm/remove) sur cppreference 