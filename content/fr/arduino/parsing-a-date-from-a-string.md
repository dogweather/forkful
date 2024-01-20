---
title:                "Analyser une date à partir d'une chaîne"
html_title:           "Clojure: Analyser une date à partir d'une chaîne"
simple_title:         "Analyser une date à partir d'une chaîne"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

# Analyser une Date d'une Chaîne de Caractères avec Arduino

## Quoi et Pourquoi?

L'analyse d'une date à partir d'une chaîne de caractères est le processus de conversion des dates codées en texte dans une forme que votre programme peut manipuler. Ce processus est essentiel pour les programmeurs pour utiliser les dates saisies par les utilisateurs de manière logique, par exemple, pour effectuer des calculs de dates.

## Comment Faire :

Voilà comment on peut analyser une date d'une chaîne de caractères avec Arduino. L'exemple ci-dessous démontre un code simple pour cette tâche.

```Arduino
#include <TimeLib.h>

void setup() {
  Serial.begin(9600);
  while (!Serial) ; // attendre l'ouverture du port série
  time_t t = parseDate("12/31/2020", "MM/dd/yyyy");
  if (t != 0) {
    Serial.println(t);
  }
}

time_t parseDate(char* sDate, char* sFormat) {
  tmElements_t tm;
  if (!strptime(sDate, sFormat, &tm)) return 0; // conversion de chaîne en tm
  return makeTime(tm); // conversion de tm en time_t
}

void loop() {
}
```

Cela permet de convertir la chaîne "31/12/2020" en un timestamp Unix, qu'il imprime.

## Plongée Profonde :

Direccion Softworks a conçu la bibliothèque TimeLib pour qu'elle soit intuitive et facile à utiliser dans Arduino. Elle supporte le format de date américain (mois/jour/année) ainsi que le format international (jour/mois/année).

Il y a d'autres alternatives pour l'analyse des dates. Les bibliothèques comme `DateStrings` et `RTClib` offrent également cette fonctionnalité. Cependant, `TimeLib` a tendance à être plus utilisé en raison de sa flexibilité et de sa simplicité.

Lorsqu'on analyse une date à partir d'une chaîne de caractères avec Arduino, il est important de comprendre que le résultat est un timestamp Unix - le nombre de secondes écoulées depuis le 1er janvier 1970 à 00:00:00 UTC (sans tenir compte des secondes intercalaires).

## Voir Aussi :

Pour plus d'informations sur l'analyse de date avec Arduino :
- [Documentation officielle d'Arduino](https://www.arduino.cc/reference/en/)
- [Github - TimeLib](https://github.com/PaulStoffregen/Time)
- [Guide de l'Horloge temps réel (RTC) avec Arduino](https://www.makerguides.com/rtc-arduino-tutorial/)