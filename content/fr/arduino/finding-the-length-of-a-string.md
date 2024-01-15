---
title:                "Trouver la longueur d'une chaîne de caractères"
html_title:           "Arduino: Trouver la longueur d'une chaîne de caractères"
simple_title:         "Trouver la longueur d'une chaîne de caractères"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

Vous vous êtes peut-être déjà demandé comment savoir la longueur d'une chaîne de caractères dans votre code Arduino. Cette information peut être utile pour effectuer certaines opérations ou pour vérifier si une entrée utilisateur est valide. Dans cet article, nous allons vous montrer comment trouver la longueur d'une chaîne de caractères en utilisant le langage de programmation Arduino.

## Comment faire

Pour trouver la longueur d'une chaîne de caractères en utilisant Arduino, nous allons utiliser une fonction appelée `strlen()`. Cette fonction n'est pas spécifique à Arduino, elle est disponible dans de nombreux langages de programmation.

Voici un exemple de code utilisant la fonction `strlen()` pour trouver la longueur d'une chaîne de caractères :

```
Arduino Code :

// Déclaration d'une chaîne de caractères
char nom[] = "John";

// Utilisation de la fonction strlen()
int longueur = strlen(nom);

// Affichage de la longueur de la chaîne
Serial.println(longueur);

```

Ce code va afficher la valeur 4, car la chaîne de caractères "John" contient 4 lettres.

Il est important de noter que la longueur retournée par la fonction `strlen()` ne prend pas en compte le caractère de fin de chaîne (\0). Par exemple, si vous utilisez la chaîne "Bonjour!" avec la fonction `strlen()`, la longueur retournée sera de 7 et non de 8.

## Plongeon en profondeur

Il existe également une autre fonction disponible sur Arduino pour trouver la longueur d'une chaîne de caractères : `sizeof()`. Cette fonction renvoie le nombre total d'octets utilisé par la variable donnée, y compris le caractère de fin de chaîne.

Prenons l'exemple suivant :

```
Arduino Code :

// Déclaration d'une chaîne de caractères
char nom[] = "John";

// Utilisation de la fonction sizeof()
int longueur = sizeof(nom);

// Affichage de la longueur de la chaîne
Serial.println(longueur);

```

Dans ce cas, la valeur affichée sera de 5, car la chaîne "John" utilise 5 octets en mémoire (4 pour les lettres du nom et 1 pour le caractère de fin de chaîne).

Cependant, il est important de noter que la fonction `sizeof()` ne fonctionne pas pour les chaînes de caractères dynamiques (déclarées à l'aide de la fonction `malloc()`). Elle ne fonctionne que pour les chaînes de caractères statiques (déclarées à l'aide de `char nom[]`).

## Voir aussi

- [Documentation officielle Arduino sur la fonction `strlen()`](https://www.arduino.cc/reference/en/language/functions/string/strlen/)
- [Documentation officielle Arduino sur la fonction `sizeof()`](https://www.arduino.cc/reference/en/language/functions/general-functions/sizeof/)