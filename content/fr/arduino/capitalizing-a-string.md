---
title:                "Mettre en majuscules une chaîne de caractères"
html_title:           "Arduino: Mettre en majuscules une chaîne de caractères"
simple_title:         "Mettre en majuscules une chaîne de caractères"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?
Mettre une chaîne en majuscules signifie transformer toutes ses lettres en majuscules. Les programmeurs le font pour diverses raisons comme normaliser les entrées des utilisateurs ou pour l'affichage des informations.

## Comment faire:
Pour transformer une chaîne en majuscules en Arduino, on utilise la méthode `toUpperCase()`. Voici un exemple simple:

```Arduino
String maChaine = "bonjour, monde";
maChaine.toUpperCase();
Serial.println(maChaine); // Affichera "BONJOUR, MONDE"
```

## APPROFONDISSEMENT
Historiquement, la mise en majuscule a été utilisée pour rendre le texte plus visible ou important. Aussi, dans certaines situations, il faut éviter les problèmes de correspondance de cas dans la comparaison des chaînes.

Une alternative à l'utilisation de `toUpperCase()` est de parcourir chaque caractère de la chaîne et de le convertir individuellement en majuscules. Cependant, la méthode `toUpperCase()` est préférée car elle est plus rapide et plus facile à utiliser.

Les détails de mise en œuvre pour `toUpperCase()` en Arduino reposent sur la définition de la bibliothèque String. Le code examine chaque caractère, vérifie s'il est en minuscules, puis le convertit en majuscules si nécessaire.

## VOIR AUSSI
Pour plus d'informations sur la manipulation de chaînes en Arduino, consultez les ressources suivantes:
- Manuel de référence Arduino "StringObject" (https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/)
- Site tutorial "Arduino and Strings" (https://startingelectronics.org/articles/arduino/strings/)