---
title:                "La concaténation de chaînes de caractères"
html_title:           "C++: La concaténation de chaînes de caractères"
simple_title:         "La concaténation de chaînes de caractères"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/concatenating-strings.md"
---

{{< edit_this_page >}}

# Qu'est-ce que c'est et pourquoi les programmeurs le font?

La concaténation de chaînes est une opération couramment utilisée en informatique qui consiste à fusionner plusieurs chaînes de caractères en une seule. Les programmeurs font cela pour manipuler et créer des chaînes plus complexes, souvent dans le but de créer du texte dynamique à afficher à l'utilisateur.

# Comment faire?

Voici un exemple de code en C++ montrant comment concaténer des chaînes de caractères:

```
#include <iostream>

using namespace std;

int main() {
  string nom = "John";
  string prenom = "Doe";
  string nom_complet = nom + prenom;
  
  cout << "Nom complet: " << nom_complet << endl;
  
  return 0;
}
```
**Output:** Nom complet: JohnDoe

# Plongée en profondeur

La concaténation de chaînes existe depuis les premiers jours de la programmation informatique, et elle est encore largement utilisée aujourd'hui. Les alternatives à la concaténation de chaînes incluent l'utilisation de tableaux de caractères ou de classes de chaînes de caractères spécifiques pour manipuler des chaînes plus complexes. C++ propose également des méthodes spécifiques telles que `append()` et `substr()` pour effectuer des opérations de concaténation.

# Voir aussi

- [Les opérateurs C++ de manipulation de chaînes de caractères](https://www.cplusplus.com/reference/string/operators/)
- [La concaténation de chaînes en C++11](https://en.cppreference.com/w/cpp/string/basic_string/operator%2B)
- [Des exemples pratiques de concaténation de chaînes en C++](https://www.geeksforgeeks.org/stdstring-class-in-c/)