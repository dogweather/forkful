---
title:                "Capitaliser une chaîne de caractères"
html_title:           "C++: Capitaliser une chaîne de caractères"
simple_title:         "Capitaliser une chaîne de caractères"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

Vous êtes peut-être en train de coder un programme dans lequel vous avez besoin de convertir une chaîne de caractères en majuscules. Peut-être que vous voulez vérifier le formatage de l'entrée utilisateur ou que vous voulez simplement afficher une ligne de texte en majuscules. Quelle que soit la raison, il est utile de savoir comment capitaliser efficacement une chaîne en C++.

## Comment faire

Il existe plusieurs façons de capitaliser une chaîne en C++. L'une des méthodes les plus simples est d'utiliser la fonction `toupper()` du header `<cctype>`. Cette fonction prend un caractère en entrée et renvoie le caractère en majuscule correspondant. En utilisant une boucle, vous pouvez parcourir chaque caractère de la chaîne et appliquer la fonction `toupper()` pour obtenir la chaîne entière en majuscules.

```C++
#include <iostream>
#include <cctype> // header pour la fonction toupper()
#include <string>

using namespace std;

int main() {
    // chaîne d'exemple
    string str = "Bonjour le monde!";
    // boucle pour parcourir chaque caractère
    for (int i = 0; i < str.length(); i++) {
        // utilisation de la fonction toupper() pour convertir en majuscules
        str[i] = toupper(str[i]);
    }
    // affichage de la chaîne en majuscules
    cout << str << endl;
    return 0;
}
```

Output:

```
BONJOUR LE MONDE!
```

Cette méthode peut également être utilisée avec une boucle `while` ou `do while` selon votre préférence. Il est également possible de créer une fonction personnalisée qui utilise la méthode ci-dessus et qui peut être réutilisée dans votre code.

## Plongée en profondeur

Il est important de noter que la fonction `toupper()` ne fonctionnera que pour les caractères alphabétiques. Pour les caractères spéciaux ou les chiffres, le résultat sera le même que l'entrée. Si vous avez besoin de convertir une chaîne en majuscules sans tenir compte de ces différences, vous pouvez utiliser la méthode `transform()` du header `<algorithm>`.

```C++
#include <iostream>
#include <algorithm> // header pour la fonction transform()
#include <string>

using namespace std;

// fonction personnalisée pour convertir une chaîne en majuscules
string capitalize(string str) {
    // boucle pour parcourir chaque caractère
    transform(str.begin(), str.end(), str.begin(), ::toupper);
    // renvoie de la chaîne en majuscules
    return str;
}

int main() {
    // chaîne d'exemple avec caractères spéciaux
    string str = "Héllo, wörld!";
    // appel de la fonction capitalize()
    str = capitalize(str);
    // affichage de la chaîne en majuscules
    cout << str << endl;
    return 0;
}
```

Output:

```
HÉLLO, WÖRLD!
```

De plus, si vous souhaitez capitaliser uniquement la première lettre d'une chaîne, vous pouvez utiliser la fonction `toupper()` seule ou avec la fonction `tolower()` pour le reste de la chaîne.

## Voir aussi

- [Documentation sur la fonction `toupper()`](https://www.cplusplus.com/reference/cctype/toupper/)
- [Documentation sur la fonction `transform()`](https://www.cplusplus.com/reference/algorithm/transform/)
- [Guide pour les débutants en C++](https://www.programiz.com/cpp-programming)