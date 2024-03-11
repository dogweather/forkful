---
date: 2024-01-20 17:37:45.962562-07:00
description: "Convertir une cha\xEEne en minuscules, c'est transformer tous les caract\xE8\
  res majuscules en leur \xE9quivalent minuscule. Les programmeurs le font souvent\
  \ pour\u2026"
lastmod: '2024-03-11T00:14:32.001931-06:00'
model: gpt-4-1106-preview
summary: "Convertir une cha\xEEne en minuscules, c'est transformer tous les caract\xE8\
  res majuscules en leur \xE9quivalent minuscule. Les programmeurs le font souvent\
  \ pour\u2026"
title: "Conversion d'une cha\xEEne de caract\xE8res en minuscules"
---

{{< edit_this_page >}}

## What & Why?
Convertir une chaîne en minuscules, c'est transformer tous les caractères majuscules en leur équivalent minuscule. Les programmeurs le font souvent pour normaliser les entrées et faciliter les comparaisons de texte sans se soucier de la casse.

## How to:
En Arduino, la classe `String` offre la méthode `toLowerCase()`. Voici comment l'utiliser :

```arduino
String message = "Bonjour, ARDUINO !";
message.toLowerCase();
Serial.begin(9600);
Serial.println(message); // Affiche "bonjour, arduino !"
```

## Deep Dive
Historiquement, le besoin de convertir des chaînes en minuscules trouve son origine dans les premiers jours de l'informatique, pour la comparaison de données textuelles et la recherche insensible à la casse. Alternativement, avant que `String` n'offre `toLowerCase()`, les programmeurs parcouraient chaque caractère et utilisaient des fonctions comme `tolower()` de la bibliothèque `ctype.h` en C. En interne, `toLowerCase()` parcourt la chaîne et convertit chaque caractère individuellement, en utilisant le tableau ASCII comme référence pour trouver l'équivalent minuscule de chaque lettre majuscule.

## See Also
Pour plus d'informations et plus de fonctions liées aux chaînes sur Arduino, consultez :

- [Arduino Reference: String](https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/)
