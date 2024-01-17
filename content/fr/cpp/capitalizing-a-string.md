---
title:                "Mettre en majuscule une chaîne de caractères"
html_title:           "C++: Mettre en majuscule une chaîne de caractères"
simple_title:         "Mettre en majuscule une chaîne de caractères"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi ?
La mise en majuscule d'une chaîne de caractères consiste à transformer chaque lettre en majuscule. Les programmeurs le font pour garantir la cohérence et la lisibilité de leurs codes.

## Comment faire :
```C++
#include <iostream>
#include <cstring>

using namespace std;

int main() {
  // Exemple de mise en majuscule d'une chaîne de caractères
  string str = "Bonjour le Monde !";
  // Utilisation de la fonction transform() de la bibliothèque <cstring>
  transform(str.begin(), str.end(), str.begin(), ::toupper);
  cout << str; // Output: BONJOUR LE MONDE !
  return 0;
}
```

## Plongée en profondeur :
La mise en majuscule des chaînes de caractères a été introduite dans les langages de programmation pour faciliter la lecture et la comparaison des codes. Une alternative à la fonction transform() est la méthode toUpper(), mais elle n'est pas disponible dans toutes les bibliothèques. L'implémentation de la mise en majuscule peut différer d'un langage de programmation à un autre, mais le principe reste le même : modifier chaque caractère en majuscule.

## Voir aussi :
- [Documentation sur la fonction transform()](https://www.cplusplus.com/reference/algorithm/transform/)
- [Documentation sur la méthode toUpper()](https://www.geeksforgeeks.org/isupper-islower-application-c/)