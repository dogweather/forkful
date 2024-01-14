---
title:                "C++: Trouver la longueur d'une chaîne de caractères"
programming_language: "C++"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

Vous êtes-vous déjà demandé pourquoi il est important de trouver la longueur d'une chaîne de caractères en programmation ? La réponse est simple : la longueur d'une chaîne peut être utilisée pour effectuer des opérations telles que la recherche, la manipulation et la validation de données.

## Comment Faire

La méthode la plus courante pour trouver la longueur d'une chaîne de caractères en C++ est d'utiliser la fonction `length()` de la bibliothèque standard. Voici un exemple de code montrant comment utiliser cette fonction :

```C++
#include <iostream>
#include <string>

using namespace std;

int main() {
    // Déclarer et initialiser une chaîne de caractères
    string phrase = "Bonjour tout le monde !";

    // Utiliser la fonction length() pour trouver la longueur de la chaîne
    int longueur = phrase.length();

    // Afficher la longueur de la chaîne
    cout << "La chaîne de caractères \"" << phrase << "\" a une longueur de " << longueur << " caractères." << endl;

    return 0;
}
```

Lorsque vous exécutez ce code, vous devriez voir la sortie suivante :

```
La chaîne de caractères "Bonjour tout le monde !" a une longueur de 23 caractères.
```

Vous pouvez également trouver la longueur d'une chaîne de caractères en utilisant une boucle `for` pour compter le nombre de caractères dans la chaîne. Cela peut être utile si vous souhaitez effectuer des opérations plus complexes sur les caractères de la chaîne.

```C++
#include <iostream>
#include <string>

using namespace std;

int main() {
    // Déclarer et initialiser une chaîne de caractères
    string mot = "développement";

    // Déclarer une variable pour stocker la longueur
    int longueur = 0;

    // Utiliser la boucle for pour compter les caractères
    for (int i = 0; mot[i] != '\0'; i++) {
        longueur++;
    }

    // Afficher la longueur de la chaîne
    cout << "Le mot \"" << mot << "\" a une longueur de " << longueur << " caractères." << endl;

    return 0;
}
```

Cette fois, vous devriez voir la sortie suivante :

```
Le mot "développement" a une longueur de 13 caractères.
```

## Plongée Profonde

Maintenant que vous savez comment trouver la longueur d'une chaîne de caractères en C++, il est important de comprendre comment cela fonctionne réellement. En interne, une chaîne de caractères est stockée comme un tableau de caractères, également appelé une chaîne de caractères. La fonction `length()` scanne simplement ce tableau et compte le nombre de caractères jusqu'à ce qu'elle atteigne le caractère de fin de chaîne `'\0'`.

Il est également important de noter que la fonction `length()` ne compte que les caractères normaux, pas les caractères spéciaux tels que les accents ou les symboles. Si vous souhaitez compter le nombre total de caractères, vous devrez utiliser une méthode plus complexe.

## Voir Aussi

- [Cplusplus.com - Fonction `length()`](https://www.cplusplus.com/reference/string/string/length/)
- [GeeksforGeeks - Trouver la longueur d'une chaîne de caractères en C++](https://www.geeksforgeeks.org/find-length-of-a-string-in-cpp/)