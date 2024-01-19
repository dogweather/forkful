---
title:                "Comparer deux dates"
html_title:           "Clojure: Comparer deux dates"
simple_title:         "Comparer deux dates"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est & Pourquoi?

Comparer deux dates est l'action de déterminer quelle date est antérieure, postérieure ou identique à l'autre. C'est crucial pour les programmeurs lorsqu'ils manipulent des données temporelles pour des événements, des rappels ou des planifications.

## Comment faire:

Voici un exemple simple de comparaison de deux dates avec Arduino.

```Arduino

#include <TimeLib.h>

Date d1 = {10, 2, 2020};
Date d2 = {9, 2, 2020};

if (d1.year > d2.year) {
  Serial.println("date1 is later than date2");
} else if (d1.year < d2.year) {
  Serial.println("date1 is earlier than date2");
} else {
  //if the year is the same
  if (d1.month > d2.month) {
   Serial.println("date1 is later than date2");
  } else if (d1.month < d2.month) {
    Serial.println("date1 is earlier than date2");
  } else {
    //if the month is the same
    if (d1.day > d2.day) {
      Serial.println("date1 is later than date2");
    } else if (d1.day < d2.day) {
      Serial.println("date1 is earlier than date2");
    } else {
      Serial.println("Both dates are identical");
    }
  }
}

```
Les sorties d'échantillon dépendraient des valeurs de date définies.

## Deep Dive:

Historiquement, la comparaison de deux dates n'est pas intrinsèquement supportée par la bibliothèque standard Arduino. Des bibliothèques tierces telles que TimeLib ont été développées à cette fin.

Il existe plusieurs approches alternatives à notre exemple. Vous pouvez, par exemple, convertir les dates en secondes depuis l'époque UNIX, ce qui leur donne une représentation numérique que vous pouvez alors facilement comparer.

Notez que les détails d'implémentation de la comparaison de deux dates peuvent varier en fonction de la précision nécessaire. Par exemple, si vous voulez juste savoir quelle date est plus récente, vous pouvez ignorer les composants d'heures, de minutes et de secondes (le cas échéant) dans votre comparaison.

## Voir Aussi: 

Pour plus d'informations, consultez le [manuel de référence Arduino](https://www.arduino.cc/reference/fr/) et pour la bibliothèque TimeLib, visitez leur [documentation](https://www.pjrc.com/teensy/td_libs_Time.html) respective. Vous pouvez également consulter des exemples en ligne sur des sites comme [arduino.cc](https://arduino.cc).