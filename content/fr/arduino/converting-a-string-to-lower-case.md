---
title:                "Conversion d'une chaîne de caractères en minuscules"
date:                  2024-01-20T17:37:45.962562-07:00
model:                 gpt-4-1106-preview
simple_title:         "Conversion d'une chaîne de caractères en minuscules"

category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/converting-a-string-to-lower-case.md"
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
