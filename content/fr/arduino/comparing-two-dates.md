---
title:                "Comparaison de deux dates"
html_title:           "Arduino: Comparaison de deux dates"
simple_title:         "Comparaison de deux dates"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous utilisez un Arduino pour un projet, il est possible que vous ayez besoin de comparer des dates. Par exemple, vous pourriez vouloir vérifier si la date actuelle est après une certaine date spécifiée ou si un événement a eu lieu avant ou après une date donnée.

## Comment faire

Pour comparer deux dates sur un Arduino, vous aurez besoin d'utiliser la bibliothèque Time.h. Cette bibliothèque vous permet de manipuler des objets de type time_t, qui représentent un nombre de secondes depuis le 1er janvier 1970. Voici un exemple de code pour comparer deux dates :

```Arduino
#include <Time.h>

time_t date1 = makeTime(0, 0, 0, 1, 1, 2021); // 1er janvier 2021 à minuit
time_t date2 = now(); // date actuelle
if (date1 > date2) {
    Serial.println("Date 1 est après Date 2.");
} else if (date1 < date2) {
    Serial.println("Date 1 est avant Date 2.");
} else {
    Serial.println("Les deux dates sont identiques.");
}
```

Le code utilise la fonction makeTime () pour créer un objet time_t à partir de valeurs spécifiées. Ensuite, il compare les deux dates en utilisant des opérateurs de comparaison. Vous pouvez également utiliser des fonctions telles que hour (), minute () et second () pour comparer des dates plus précises.

## Plongée en profondeur

Il est important de noter que Time.h utilise le fuseau horaire UTC (Temps Universel Coordonné). Cela signifie que vous devez prendre en compte le décalage horaire de votre emplacement pour obtenir la date locale correcte. Pour cela, vous pouvez utiliser la fonction adjustTime () qui ajoute ou soustrait un certain nombre de secondes pour ajuster la date.

De plus, si vous avez besoin de comparer des dates sur de longues périodes, il est recommandé de convertir les time_t en un nombre de jours. Cela peut être fait en divisant le time_t par le nombre de secondes dans une journée (86400).

## Voir aussi

- Référence pour la bibliothèque Time.h : https://www.arduino.cc/reference/en/libraries/time/
- Tutoriel sur les dates et heures avec Arduino : https://create.arduino.cc/projecthub/shubhamkumar_/arduino-time-date-tutorial-0980bb
- Support de la communauté Arduino pour toute question ou aide supplémentaire : https://forum.arduino.cc/