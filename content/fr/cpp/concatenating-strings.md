---
title:                "Concaténation de chaînes"
html_title:           "C: Concaténation de chaînes"
simple_title:         "Concaténation de chaînes"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/concatenating-strings.md"
---

{{< edit_this_page >}}

## Qu'est-ce que? Et pourquoi?

La concaténation de chaînes est la pratique consistant à combiner deux chaînes ou plus en une seule. Les programmeurs utilisent cette méthode pour manipuler les données textuelles et rendre le code plus concis et lisible.
 
## Comment faire:

La concaténation de chaînes en C++ peut être réalisée à l'aide de l'opérateur '+'. Voici un exemple:
```C++
#include<iostream>
#include<string>

int main(){
    std::string str1 = "Bonjour, ";
    std::string str2 = "monde!";
    std::string str3 = str1 + str2;
    std::cout << str3;
    return 0;
}
```
Résultat: `Bonjour, monde!`

## Approfondissement

Historiquement, la concaténation de chaînes remonte aux premiers jours de la programmation, bien avant la création du C++. Au fil du temps, diverses méthodes de concaténation de chaînes ont été développées pour différentes langues.

Alternativement, en C++, vous pouvez également utiliser la méthode `append()` de la classe `std::string` pour concaténer des chaînes.

```C++
std::string str4 = "Comment ";
str4.append("ça va?");
std::cout << str4;
```
Résultat : `Comment ça va?`

En ce qui concerne les détails de mise en œuvre, l'opérateur '+' et la méthode `append()` alloueront tous deux un nouvel espace mémoire pour accueillir la nouvelle chaîne combinée et copieront les données de la chaîne source dans ce nouvel emplacement. Cette opération a une complexité temporelle en O(n), n étant le nombre total de caractères dans la chaîne cible.

## À voir également

- Cours détaillé sur `std::string` en C++ : https://www.cplusplus.com/reference/string/string/
- Different ways to concatenate strings in C++ : https://www.geeksforgeeks.org/concatenating-strings-in-c-cpp/
- Comparaison des performances des méthodes de concaténation de chaînes : https://baptiste-wicht.com/posts/2012/12/cpp-benchmark-string-concatenation.html