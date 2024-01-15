---
title:                "Convertir une date en chaîne de caractères"
html_title:           "Arduino: Convertir une date en chaîne de caractères"
simple_title:         "Convertir une date en chaîne de caractères"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi
Tu te demandes peut-être pourquoi il est important de convertir une date en chaîne de caractères pour ton projet Arduino. Eh bien, cela peut être utile pour l'affichage d'informations sur un écran LCD ou pour l'enregistrement de données dans un fichier.

## Comment faire
Pour convertir une date en chaîne de caractères sur Arduino, tu peux utiliser la fonction `String()` et la fonction `sprintf()`. Voici un exemple de code pour obtenir la date et l'heure actuelles sous forme de chaîne de caractères :

```Arduino
#include <TimeLib.h> // inclus la bibliothèque Time
#include <TimeAlarms.h> // inclus la bibliothèque TimeAlarms

void setup() {
  Serial.begin(9600); // initialise la communication série
  setTime(14, 20, 0, 1, 1, 2021); // définit la date et l'heure actuelles
}

void loop() {
  // utilise la fonction String() pour convertir la date en chaîne de caractères
  String date = String(day()) + "/" + String(month()) + "/" + String(year());
  // utilise la fonction sprintf() pour convertir l'heure en chaîne de caractères
  char heure[9];
  sprintf(heure, "%02d:%02d:%02d", hour(), minute(), second());
  // affiche la date et l'heure sur le moniteur série
  Serial.print("Date: ");
  Serial.println(date);
  Serial.print("Heure: ");
  Serial.println(heure);
  delay(1000); // attend une seconde pour afficher les informations suivantes
}
```

Voici l'exemple de sortie sur le moniteur série :

```
Date: 1/1/2021
Heure: 14:20:00
```

## Deep Dive
Maintenant que tu sais comment convertir une date en chaîne de caractères sur Arduino, voici quelques points à prendre en compte :

- La fonction `sprintf()` utilise un format de chaîne similaire à celui de la fonction `printf()` en langage C.
- Tu peux utiliser différentes spécifications de format pour afficher des dates et des heures dans différents formats. Par exemple, `%d` pour les jours et les mois, `%02d` pour les heures, minutes et secondes avec le zéro ajouté si seulement un chiffre est présent, etc.
- La bibliothèque TimeAlarms (incluse dans l'exemple de code) peut être utile pour programmer des alarmes basées sur des dates et des heures.
- N'oublie pas de définir la date et l'heure correctes dans la fonction `setTime()` pour obtenir les informations actuelles.

## Voir aussi
- [Documentation officielle de la fonction String()](https://www.arduino.cc/reference/en/language/variables/data-types/string/)
- [Documentation officielle de la fonction sprintf()](https://www.arduino.cc/reference/en/language/functions/communication/serial/sprintf/)
- [Tutoriel sur la bibliothèque Time pour Arduino](https://www.arduino.cc/reference/en/libraries/time/)
- [Tutoriel sur la bibliothèque TimeAlarms pour Arduino](https://www.arduino.cc/en/Reference/TimeAlarms)