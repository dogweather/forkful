---
title:                "Conversion d'une chaîne en minuscules"
html_title:           "Java: Conversion d'une chaîne en minuscules"
simple_title:         "Conversion d'une chaîne en minuscules"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/java/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi le faire?
La conversion d'une chaîne de caractères en minuscules est une opération couramment utilisée en programmation pour changer toutes les lettres majuscules en lettres minuscules dans une chaîne de caractères donnée. Cela peut être utile pour comparer des chaînes de caractères de manière insensible à la casse ou pour assurer une uniformité dans le formatage des données.

## Comment le faire:
Voici un exemple de code en Java pour convertir une chaîne de caractères en minuscules:
```
String str = "HELLO WORLD";
String lowerCaseStr = str.toLowerCase();
System.out.println(lowerCaseStr);
```
La sortie de ce code serait "hello world". Comme vous pouvez le voir, la méthode `toLowerCase()` est utilisée sur la chaîne de caractères d'origine et une nouvelle chaîne de caractères en minuscules est assignée à la variable `lowerCaseStr`.

## Plongée en profondeur:
La conversion de chaînes de caractères en minuscules est utilisée depuis longtemps dans les programmes informatiques. Cela remonte aux premiers systèmes informatiques qui n'utilisaient que des lettres majuscules. Cependant, avec l'avènement de l'informatique moderne, il est devenu courant d'avoir des chaînes de caractères contenant à la fois des lettres majuscules et minuscules, rendant ainsi cette opération nécessaire.

Il existe plusieurs alternatives pour convertir une chaîne de caractères en minuscules en Java, telles que l'utilisation de la bibliothèque Apache Commons ou l'utilisation de la méthode `toLowerCase(Locale)` pour spécifier une région ou une langue spécifique pour la conversion.

Au niveau de l'implémentation, la méthode `toLowerCase()` utilise la norme unicode pour effectuer la conversion, ce qui signifie qu'elle prend en compte les caractères spéciaux et les accents.

## Voir aussi:
- Documentation officielle Java pour la méthode `toLowerCase()`: https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#toLowerCase()
- Bibliothèque Apache Commons: https://commons.apache.org/proper/commons-lang/
- Liste des régions et langues prises en charge pour la méthode `toLowerCase(Locale)`: https://docs.oracle.com/javase/8/docs/api/java/util/Locale.html