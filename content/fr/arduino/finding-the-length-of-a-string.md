---
title:                "Trouver la longueur d'une chaîne"
html_title:           "Arduino: Trouver la longueur d'une chaîne"
simple_title:         "Trouver la longueur d'une chaîne"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?
Trouver la longueur d'une chaîne de caractères est une tâche essentielle pour les programmeurs. Cela leur permet de vérifier et de manipuler facilement les textes dans leurs programmes. Cette opération est également utile pour les entrées utilisateur et les fonctions de traitement des chaînes de caractères.

## Comment faire:
```
Arduino
char string[] = "Bonjour";
int length = strlen(string);
Serial.println(length);  // Output: 7
```

Il est important de noter que la longueur d'une chaîne de caractères est comptée à partir de 0, ce qui signifie que la longueur de la chaîne "Bonjour" sera de 6 et non pas 7, car le premier caractère est compté comme 0.

```
Arduino
int length = sizeof(string);  // Output: 8
```

La méthode ```sizeof()``` renvoie la taille en bytes d'une variable, donc la longueur de la chaîne "Bonjour" sera de 8, car elle inclut également l'espace pour le caractère de fin de chaîne.

## Plongée en profondeur:
Trouver la longueur d'une chaîne de caractères n'était pas une opération aussi simple dans les premiers langages de programmation. Les programmeurs devaient utiliser des boucles pour compter le nombre de caractères dans une chaîne. Cependant, avec l'introduction de la bibliothèque de fonctions standard, la fonction ```strlen()``` a été créée pour simplifier cette tâche.

Il existe également d'autres méthodes pour trouver la longueur d'une chaîne de caractères, telles que la méthode ```length()```, qui renvoie le nombre de caractères dans une chaîne et exclut le caractère de fin de chaîne, et la méthode ```size()```, qui renvoie la taille totale de la chaîne en bytes.

Il est important de noter que les chaînes de caractères sont des tableaux de caractères en mémoire, et la première adresse de la chaîne est utilisée pour stocker le nombre de caractères dans la chaîne.

## Voir aussi:
- [La fonction strlen() dans la bibliothèque de fonctions standard C](https://www.tutorialspoint.com/c_standard_library/string_h.htm)
- [Comparaison de différentes méthodes pour trouver la longueur d'une chaîne en C++](https://www.geeksforgeeks.org/finding-length-of-a-string-in-cpp/)