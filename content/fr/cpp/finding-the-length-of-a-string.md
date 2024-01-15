---
title:                "Trouver la longueur d'une chaîne de caractères"
html_title:           "C++: Trouver la longueur d'une chaîne de caractères"
simple_title:         "Trouver la longueur d'une chaîne de caractères"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

Vous vous demandez probablement pourquoi il est important de connaître la longueur d'une chaîne de caractères lors de la programmation en C++. Eh bien, c'est simple : en connaissant la longueur d'une chaîne, vous pouvez effectuer des manipulations et des opérations sur les caractères qui la composent, ce qui est souvent nécessaire dans la résolution de problèmes complexes.

## Comment Faire

Il existe deux façons de trouver la longueur d'une chaîne de caractères en C++ : en utilisant la fonction `strlen()` de la bibliothèque standard ou en utilisant une boucle pour parcourir la chaîne un caractère à la fois. Voici un exemple de code utilisant `strlen()` :

```C++
#include <iostream>
#include <cstring>

using namespace std;

int main() {
    char str[] = "Bonjour";
    int length = strlen(str);
    cout << "La longueur de la chaîne est de : " << length << " caractères." << endl;
    return 0;
}
```

Ce code déclare une chaîne de caractères "Bonjour" et utilise la fonction `strlen()` pour calculer sa longueur, qui est ensuite affichée à l'écran. Le résultat sera "La longueur de la chaîne est de : 7 caractères."

Si vous préférez utiliser une boucle pour trouver la longueur d'une chaîne, voici un exemple de code :

```C++
#include <iostream>

using namespace std;

int main() {
    char str[] = "Bonjour";
    int length = 0;
    while (str[length] != '\0') {
        length++;
    }
    cout << "La longueur de la chaîne est de : " << length << " caractères." << endl;
    return 0;
}
```

Dans ce code, nous initialisons la longueur à zéro puis utilisons une boucle pour parcourir la chaîne jusqu'à ce que nous atteignions le caractère de fin de chaîne `'\0'`. À chaque tour de boucle, nous incrémentons la longueur, ce qui nous donne finalement la longueur totale de la chaîne. Le résultat sera le même que dans l'exemple précédent.

## Plongée en Profondeur

Si vous voulez en savoir plus sur la façon dont la fonction `strlen()` fonctionne réellement, nous pouvons regarder son implémentation. En C++, cette fonction est définie de la manière suivante :

```C++
size_t strlen(const char *str) {
    size_t length = 0;
    while (*str++) {
        length++;
    }
    return length;
}
```

Nous pouvons voir que cette fonction utilise également une boucle pour parcourir la chaîne de caractères, mais elle utilise un pointeur pour accéder à chaque caractère plutôt qu'un index. Elle continue à incrémenter la longueur tant que la valeur pointée par le pointeur n'est pas nulle (équivalent de `'\0'`). Elle retourne ensuite la longueur totale de la chaîne. Cette fonction est optimisée pour être très efficace et est largement utilisée dans le code C++.

## Voir aussi

Si vous souhaitez en savoir plus sur les chaînes de caractères en C++, vous pouvez consulter ces liens :

- [Documentation sur la fonction `strlen()`](https://www.cplusplus.com/reference/cstring/strlen/)
- [Différence entre `char *` et `const char *`](https://stackoverflow.com/questions/41233615/difference-between-char-and-const-char)
- [Tutoriel Scratch de Débutant pour la Programmation C++](https://www.scratchapixel.com/lessons/advanced-cpp/scope-and-parameter-passing-by-reference)