---
title:    "C++: Convertir une chaîne en minuscules"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Pourquoi

Dans de nombreuses situations en programmation, il peut être utile de convertir une chaîne de caractères en minuscules. Cela peut faciliter la manipulation et la comparaison de chaînes de caractères, en s'assurant que les caractères majuscules et minuscules ne seront pas pris en compte.

## Comment faire 

```C++
#include <iostream>
#include <string>
#include <algorithm> // Inclure la bibliothèque d'algorithmes

int main() {
  // Création d'une chaîne de caractères originale
  std::string str = "PROGRAMMATION EN C++";

  // Utilisation de la fonction transform de la bibliothèque d'algorithmes pour convertir la chaîne en minuscules
  std::transform(str.begin(), str.end(), str.begin(), ::tolower); 

  // Affichage de la chaîne convertie en minuscules
  std::cout << str << '\n';
}

// Output: programmation en c++
```

## Plongée profonde

Pour comprendre comment fonctionne la conversion en minuscules, il faut savoir que chaque caractère dans la chaîne est représenté par un code ASCII. Les lettres majuscules et minuscules ont des codes ASCII différents, ce qui permet à l'ordinateur de les distinguer. Lorsque l'on utilise la fonction `transform` de la bibliothèque d'algorithmes, elle parcourt chaque caractère de la chaîne et utilise la fonction `tolower` pour convertir le code ASCII en correspondant à une lettre minuscule.

## Voir aussi

- [Documentation sur la fonction `transform` (en anglais)](https://en.cppreference.com/w/cpp/algorithm/transform)
- [Tableau des codes ASCII](https://www.ascii-code.com/)