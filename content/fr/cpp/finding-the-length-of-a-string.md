---
title:                "Trouver la longueur d'une chaîne"
html_title:           "Go: Trouver la longueur d'une chaîne"
simple_title:         "Trouver la longueur d'une chaîne"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Trouver la longueur d'une chaîne en C++ : un guide compréhensible

## Quoi & pourquoi ?

Trouver la longueur d'une chaîne signifie déterminer le nombre de caractères qu'elle contient. Les programmeurs le font souvent pour manipuler les données de chaîne et pour écrire du code plus sûr et plus efficace.

## Comment faire :

Voici un exemple simple pour trouver la longueur d'une chaîne en C++ à l'aide de la fonction `size()` ou de la fonction `length()`.

```C++
#include <iostream>
#include <string>

int main() {
    std::string str = "Bonjour le monde";

    std::cout << "La longueur de la chaîne est : " << str.size() << std::endl;

    std::cout << "La longueur de la chaîne est aussi : " << str.length() << std::endl;

    return 0;
}
```

L'output sera :

```C++
La longueur de la chaîne est : 17
La longueur de la chaîne est aussi : 17
```

Dans le code ci-dessus, `size()` et `length()` retournent la longueur de la chaîne `str`, qui est 17.

## Plongée profonde 

Historiquement, la longueur d'une chaîne C++ était calculée à l'aide d'une boucle `while` ou `for`, en la parcourant jusqu'à atteindre le caractère nul (`\0`). Cependant, ce processus est devenu plus facile et plus sûr grâce à des fonctions intégrées comme `size()` et `length()`.

Derrière les scènes, `size()` et `length()` se comportent de la même façon. Ils retournent le nombre de caractères dans une chaîne sans compter le caractère nul de terminaison.

Les alternatives à `size()` et `length()` incluent l'utilisation de l'opérateur `sizeof()` pour les tableaux de caractères et la fonction `strlen()` pour les chaînes C. Notez cependant que `sizeof()` et `strlen()` peuvent donner des résultats différents, car `sizeof()` compte aussi le caractère nul de terminaison.

Il est important de choisir la bonne méthode en fonction des besoins spécifiques de votre programme.

## Voir aussi 

Pour plus d'informations, consultez les sources suivantes :

- [Documentation C++ - std::string::size](http://www.cplusplus.com/reference/string/string/size/) 
- [Documentation C++ - std::string::length](http://www.cplusplus.com/reference/string/string/length/)
- [Documentation C++ - std::strlen](https://www.cplusplus.com/reference/cstring/strlen/)