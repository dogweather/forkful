---
title:                "Obtenir la date actuelle"
html_title:           "Arduino: Obtenir la date actuelle"
simple_title:         "Obtenir la date actuelle"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes en train de construire un projet avec votre Arduino, il est probable que vous ayez besoin de connaître la date et l'heure actuelles. Que vous souhaitiez afficher la date sur un écran LCD ou enregistrer des données avec un horodatage, obtenir la date actuelle peut être très utile.

## Comment faire

Pour obtenir la date et l'heure actuelles sur votre Arduino, vous pouvez utiliser la bibliothèque intégrée "Time". Voici un exemple de code pour obtenir la date et l'heure actuelles :

```Arduino
#include <Time.h> // Importer la bibliothèque Time

void setup() {

  Serial.begin(9600); // Initialiser la communication série
  while (!Serial) {
    ; // Attendre que la communication série soit établie
  }

  setTime(9, 30, 0, 10, 8, 2021); // Définir manuellement la date et l'heure actuelles (heure, minute, seconde, jour, mois, année)

}

void loop() {

  // Obtenir la date et l'heure actuelles
  Serial.println("La date et l'heure actuelles sont :");
  Serial.print(day()); // Jour en cours
  Serial.print("/");
  Serial.print(month()); // Mois en cours
  Serial.print("/");
  Serial.print(year()); // Année en cours
  Serial.print(" ");
  Serial.print(hour()); // Heure en cours (format 24 heures)
  Serial.print(":");
  Serial.print(minute()); // Minute en cours
  Serial.print(":");
  Serial.println(second()); // Seconde en cours

  delay(1000); // Attendre 1 seconde avant de répéter

}
```

Lorsque vous téléversez ce code sur votre Arduino et ouvrez le moniteur série (9600 bauds), vous devriez voir la date et l'heure actuelles s'afficher en continu.

## Plongée en profondeur

La bibliothèque "Time" utilise l'horloge temps réel (RTC) intégrée à votre carte Arduino pour obtenir la date et l'heure actuelles. Cela signifie que votre Arduino doit être alimenté en permanence pour conserver l'heure, même lorsque vous le débranchez. Si vous avez besoin de changer manuellement la date et l'heure, vous pouvez utiliser la fonction `setTime()` comme dans l'exemple précédent.

La bibliothèque "Time" offre également d'autres fonctions pratiques pour travailler avec le temps, telles que `hour()`, `minute()` et `second()` pour obtenir séparément l'heure, la minute et la seconde actuelles.

## Voir aussi

- [Documentation officielle de la bibliothèque Time](https://www.arduino.cc/reference/en/libraries/time/)
- [Tutorial sur comment utiliser la bibliothèque Time](https://playground.arduino.cc/Code/time/)