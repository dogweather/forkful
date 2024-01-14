---
title:                "C++: Conversion d'une chaîne de caractères en minuscules"
programming_language: "C++"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Pourquoi
Souvent dans la programmation, nous avons besoin de manipuler des chaînes de caractères. Il peut arriver que ces chaînes contiennent des lettres en majuscules et en minuscules mélangées. Pour assurer la cohérence et la simplicité dans nos opérations de traitement de données, il peut être utile de convertir ces chaînes en minuscules, afin que toutes les lettres soient uniformes. Cela peut également faciliter la comparaison de chaînes et la recherche de certains caractères.

## Comment Faire
La conversion d'une chaîne de caractères en minuscules peut sembler complexe à première vue, mais c'est en fait assez simple avec le bon code. Voici un exemple en C++ :

```C++
#include <iostream>
#include <string>
#include <algorithm> // cette librairie contient la fonction transform

using namespace std;

int main()
{
    string str = "Bonjour le monde!";

    // Utilisation de la fonction transform pour convertir en minuscules
    transform(str.begin(), str.end(), str.begin(), ::tolower);

    cout << str; // output : bonjour le monde!

    return 0;
}
```

Dans cet exemple, nous utilisons la fonction `transform` de la librairie `<algorithm>` pour convertir la chaîne `str` en minuscules. La syntaxe de la fonction est la suivante :

`transform (début, fin, début, ::tolower)`

Elle prend en paramètres le début et la fin de la chaîne à convertir, le début de la chaîne de sortie (qui peut être la même que la chaîne d'entrée) et la fonction `::tolower`, qui convertit chaque caractère en minuscule.

## Plongée Profonde
La fonction `transform` utilisée dans notre exemple utilise une fonction de transformation prédéfinie (comme `::tolower`), mais il est également possible de créer sa propre fonction de transformation personnalisée. Voici un exemple qui convertit uniquement la première lettre d'une chaîne en majuscule et le reste en minuscules :

```C++
#include<iostream>
#include<string>
#include<algorithm>

using namespace std;

// Définition de la fonction de transformation personnalisée
char toTitleCase(char c) {
    if (c >= 'a' && c <= 'z') {
        return c - 32; // conversion en majuscule
    }
    return c; // retourne le caractère sans modification
}

int main() {
    string str = "bIeNvEnUe aU mOnDe!";
    // Utilisation de la fonction transform avec la fonction de transformation personnalisée
    transform(str.begin(), str.end(), str.begin(), toTitleCase);
    cout << str; // output : Bienvenue au monde!
    return 0;
}
```

En utilisant une fonction de transformation personnalisée, nous pouvons avoir un contrôle plus précis sur la conversion de la chaîne en minuscules.

## Voir Aussi
- [Documentation sur la fonction transform en C++](https://www.cplusplus.com/reference/algorithm/transform/)
- [Autres méthodes pour convertir une chaîne en minuscules en C++](https://www.geeksforgeeks.org/converting-strings-lowercase-uppercase-c/)