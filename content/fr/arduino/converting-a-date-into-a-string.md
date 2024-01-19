---
title:                "Convertir une date en chaîne de caractères"
html_title:           "Gleam: Convertir une date en chaîne de caractères"
simple_title:         "Convertir une date en chaîne de caractères"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## C'est quoi & Pourquoi ?
La conversion d'une date en chaîne est le processus de transformation d'un objet de date en une chaîne de caractères. Les programmeurs l'utilisent pour représenter la date d'une manière plus lisible ou pour une utilisation dans des formats qui ne supportent que les chaînes de caractères.

## Comment faire :
Voici comment vous pouvez convertir une date en chaîne en Arduino. 

```Arduino
#include <TimeLib.h> 

void setup() {
  Serial.begin(9600);
  setTime(22,30,00,5,11,1955); // set time to Saturday 8:30:00am, Nov 5 1955
}

void loop() {
  digitalClockDisplay();
  delay(1000);
}

void digitalClockDisplay() {
  char buffer[50];
  sprintf(buffer, "%02d:%02d:%02d %02d/%02d/%4d", hour(), minute(), second(), month(), day(), year());
  Serial.println(buffer);
}
```

En exécutant ce code, vous verrez l'output suivant :

```Arduino
22:30:00 05/11/1955
``` 

## Plus d'informations :
La manipulation des dates a toujours été une partie essentielle de la programmation. Il y a plusieurs façons de convertir une date en chaîne en Arduino. Ici, nous avons utilisé la fonction sprintf() pour formater notre date. On peut également utiliser la PROGMEM pour stocker le format de date, ce qui peut aider à économiser la mémoire.

En ce qui concerne les alternatives, l'Arduino fournit également des fonctions pour obtenir le jour de la semaine, le jour de l'année, etc. Si vous avez besoin d'une représentation de date plus complexe, il existe des bibliothèques comme la RTClib qui peuvent gérer des formats de date plus sophistiqués.

## Voir aussi :
- La documentation officielle de TimeLib: http://playground.arduino.cc/code/time
- RTClib pour des formats de date plus avancés : https://github.com/adafruit/RTClib
- Guide de la PROGMEM : https://www.arduino.cc/reference/fr/language/variables/utilities/progmem/