---
title:                "C++: Convertir une chaîne en minuscules"
simple_title:         "Convertir une chaîne en minuscules"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Pourquoi

Il y a plusieurs raisons pour lesquelles vous pourriez avoir besoin de convertir une chaîne de caractères en minuscules dans votre code C++. Cela peut vous permettre de uniformiser l'affichage du texte, de faciliter la comparaison de chaînes de caractères ou encore de rendre votre code plus lisible.

## Comment faire

Pour convertir une chaîne de caractères en minuscules en C++, vous pouvez utiliser la fonction `tolower()` de la bibliothèque standard `<cctype>`. Voici un exemple de code pour illustrer son utilisation :

```c++
#include <iostream>
#include <string>
#include <cctype>

using namespace std;

int main() {
    // Déclaration d'une chaîne de caractères
    string str = "Hello World";

    // Parcourir chaque caractère de la chaîne et le convertir en minuscule
    for (int i = 0; i < str.length(); i++) {
        str[i] = tolower(str[i]);
    }

    // Affichage de la chaîne convertie en minuscules
    cout << str << endl;

    return 0;
}
```

Output:
`hello world`

Vous pouvez également utiliser la fonction `transform()` de la bibliothèque `<algorithm>` pour convertir une chaîne de caractères en minuscules de manière plus efficace. Voici un autre exemple de code pour vous montrer son utilisation :

```c++
#include <iostream>
#include <string>
#include <algorithm>

using namespace std;

int main() {
    // Déclaration d'une chaîne de caractères
    string str = "Bonjour le Monde";

    // Utilisation de la fonction transform() pour convertir la chaîne en minuscules
    transform(str.begin(), str.end(), str.begin(), ::tolower);

    // Affichage de la chaîne convertie en minuscules
    cout << str << endl;

    return 0;
}
```

Output:
`bonjour le monde`

## Plongée en profondeur

Les fonctions `tolower()` et `transform()` ne sont pas les seules façons de convertir une chaîne de caractères en minuscules en C++. Vous pouvez également écrire votre propre fonction en utilisant des boucles et des conditions pour parcourir chaque caractère de la chaîne et le convertir en minuscule. Cependant, il est important de noter que ces deux fonctions de la bibliothèque standard sont plus rapides et plus efficaces.

De plus, il est possible de convertir non seulement les caractères alphanumériques, mais aussi les caractères spéciaux en minuscules en utilisant les fonctions de la bibliothèque standard.

## Voir aussi

- [Documentation de la fonction `tolower()`](https://www.cplusplus.com/reference/cctype/tolower/)
- [Documentation de la fonction `transform()`](https://www.cplusplus.com/reference/algorithm/transform/)