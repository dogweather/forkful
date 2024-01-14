---
title:    "Arduino: Comparer deux dates"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Pourquoi
L'utilisation de comparaisons de dates dans la programmation Arduino peut être utile pour une variété de projets, tels que la surveillance du temps écoulé entre deux événements ou la détermination de la date et de l'heure actuelles.

## Comment faire
La comparaison de dates en utilisant Arduino est relativement simple. Tout d'abord, vous devrez définir deux variables pour stocker les dates que vous souhaitez comparer. Par exemple, vous pouvez les nommer "date1" et "date2".

```Arduino
int date1 = 10222021; // 10 octobre 2021
int date2 = 09252021; // 25 septembre 2021

if (date1 > date2) {
    Serial.println("Date 1 est plus récente que Date 2");
} else if (date2 > date1) {
    Serial.println("Date 2 est plus récente que Date 1");
} else {
    Serial.println("Les deux dates sont identiques");
}
```

Dans cet exemple, nous avons utilisé des nombres pour représenter les dates, mais vous pouvez également utiliser des chaînes de caractères ou des objets Date pour stocker les dates. Assurez-vous simplement que les dates sont dans un format comparable.

## Plongée en profondeur
La comparaison de dates peut devenir plus complexe lorsqu'il s'agit de prendre en compte des facteurs tels que les années bissextiles et les différents nombres de jours dans un mois. Il est important de bien comprendre la logique derrière la comparaison des dates pour éviter les erreurs.

Une façon de simplifier la comparaison de dates est d'utiliser la bibliothèque Time. Cette bibliothèque fournit des fonctions utiles pour récupérer et manipuler les dates et les heures dans Arduino.

## Voir aussi
- [Documentation officielle Arduino sur la bibliothèque Time](https://www.arduino.cc/reference/en/libraries/time/)
- [Tutoriel sur la bibliothèque Time en français](https://eskimon.fr/tuto-arduino-105-la-bibliotheque-time/)
- [Exemple de projet utilisant la comparaison de dates](https://www.ardumotive.com/how-to-use-a-real-time-clock-with-arduino.html)