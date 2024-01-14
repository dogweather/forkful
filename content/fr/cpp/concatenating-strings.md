---
title:                "C++: Concaténation de chaînes de caractères"
simple_title:         "Concaténation de chaînes de caractères"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/concatenating-strings.md"
---

{{< edit_this_page >}}

## Pourquoi

Concaténer des chaînes de caractères peut sembler être une tâche simple en C++, mais cela peut en réalité avoir de nombreux avantages. Que ce soit pour faciliter la manipulation des données ou pour améliorer les performances de votre programme, apprendre à concaténer des chaînes peut être très utile pour tout développeur C++.

## Comment le faire

Pour concaténer des chaînes de caractères en C++, il suffit d'utiliser l'opérateur "+" entre les deux chaînes à concaténer. Par exemple, pour concaténer la chaîne "Bonjour" avec la chaîne "tout le monde", on écrirait :

```C++
std::string salutation = "Bonjour";
std::string message = salutation + " tout le monde";
```

L'opérateur "+" peut également être utilisé pour concaténer des variables avec des chaînes de caractères. Voici un exemple où nous concaténons la valeur d'une variable avec une chaîne de caractères pour créer un message personnalisé :

```C++
int age = 25;
std::string message = "J'ai " + std::to_string(age) + " ans.";
```

Lorsque vous exécutez ce code, la valeur de la variable age sera convertie en chaîne de caractères grâce à la fonction "std::to_string" avant d'être concaténée.

## Plongée en profondeur

Il est important de noter que lorsqu'on concatène des chaînes de caractères en C++, une nouvelle chaîne est créée à chaque fois. Cela signifie qu'en concaténant plusieurs chaînes dans une boucle, vous risquez d'avoir des problèmes de performances. Dans de tels cas, il peut être utile d'utiliser la classe "std::stringstream" qui permet de concaténer efficacement des chaînes de caractères en utilisant des flux.

Il est également possible de concaténer des chaînes avec des caractères ou d'autres types de données en utilisant l'opérateur "+=". Cela peut être utile lorsque vous voulez ajouter un caractère ou une valeur après avoir déjà concaténé des chaînes. Par exemple :

```C++
std::string message = "Bonjour";
message += " tout le monde !";
```

## Voir aussi

- [Tutoriel sur les opérateurs en C++](https://www.tutorialspoint.com/cplusplus/cpp_operators.htm)
- [Documentation sur la classe std::stringstream](https://en.cppreference.com/w/cpp/io/basic_stringstream)
- [Exemples de concaténation de chaînes en C++](https://www.programiz.com/cpp-programming/string-concatenation)