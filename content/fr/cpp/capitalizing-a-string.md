---
title:    "C++: Capitaliser une chaîne de caractères."
keywords: ["C++"]
---

{{< edit_this_page >}}

# Pourquoi capitaliser une chaîne de caractères en C++ ?

La capitalisation d'une chaîne de caractères en C++ signifie mettre en majuscules la première lettre de chaque mot dans une phrase. Bien qu'il puisse sembler trivial, il existe plusieurs raisons pour lesquelles un programmeur pourrait vouloir capitaliser une chaîne de caractères.

## Comment faire

Voici un exemple de code simple en C++ pour capitaliser une chaîne de caractères :

```C++
#include <iostream>
#include <string>

using namespace std;

string capitalize(string s) {
    // Séparer la phrase en mots individuels
    for (int i = 0; i < s.length(); i++) {
        // Mettre en majuscules la première lettre de chaque mot
        if (i == 0 || s[i - 1] == ' ') {
            s[i] = toupper(s[i]);
        }
    }

    return s;
}

int main() {
    string phrase = "je suis un programmeur C++";
    cout << capitalize(phrase) << endl;

    return 0;
}
```

Output : "Je Suis Un Programmeur C++"

## Plongée en profondeur

Il existe plusieurs façons de capitaliser une chaîne de caractères en C++ en utilisant des fonctions de la bibliothèque standard telles que `toupper()` ou `transform()`. Cependant, il est important de prendre en compte les différences entre les langues et les encodages de caractères lors de la manipulation de chaînes de caractères.

De plus, il est également utile de comprendre les performances de différentes méthodes de capitalisation de chaînes et de choisir la plus efficace pour votre programme.

# Voir aussi

- https://www.geeksforgeeks.org/capitalize-string-in-cpp/
- https://www.techiedelight.com/capitalize-string-cpp/
- https://docs.microsoft.com/fr-fr/cpp/standard-library/capitalize