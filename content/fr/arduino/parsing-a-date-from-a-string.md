---
title:                "Analyse d'une date à partir d'une chaîne de caractères"
date:                  2024-01-20T15:34:13.913490-07:00
html_title:           "Arduino: Analyse d'une date à partir d'une chaîne de caractères"
simple_title:         "Analyse d'une date à partir d'une chaîne de caractères"

category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Le "parsing" d'une date, c'est extraire et convertir l'information de date d'une chaîne de caractères. On le fait pour traiter des dates dans le format désiré, pour les enregistrer ou les comparer.

## Comment faire :
Voici un exemple simple pour parser une date depuis une chaîne de caractères avec Arduino :

```cpp
#include <Wire.h>
#include <RTClib.h>

RTC_DS1307 rtc;

void setup() {
  Serial.begin(9600);
  if (!rtc.begin()) {
    Serial.println("Couldn't find RTC");
    while (1);
  }
}

void loop() {
  DateTime now = rtc.now();

  String dateStr = "DD/MM/YYYY";
  dateStr.replace("DD", String(now.day()));
  dateStr.replace("MM", String(now.month()));
  dateStr.replace("YYYY", String(now.year()));

  Serial.println(dateStr);
  delay(1000);
}
```

Sortie attendue :
```
04/03/2023
```

## Exploration approfondie
Historiquement, la gestion des dates en informatique est complexe, notamment à cause des multiples formats et des fuseaux horaires. En Arduino, les bibliothèques comme `RTClib` simplifient le travail en offrant des fonctions pratiques pour manipuler les dates et heures. Il existe d'autres approches comme l'utilisation de fonctions personnalisées ou d'autres bibliothèques, chacune avec ses trade-offs entre mémoire, précision et fonctionnalités.

Lorsqu'on travaille avec des chaînes de caractères pour les dates, on doit s'assurer que le format est bien respecté pour éviter les erreurs de conversion. L’implémentation peut varier selon le type de puce RTC (Real Time Clock) utilisée, mais les principes de base restent les mêmes.

## Voir aussi
- [Documentation de la bibliothèque RTClib](https://github.com/adafruit/RTClib)
- [Arduino Time Library](https://www.pjrc.com/teensy/td_libs_Time.html)
- [Guide sur les bases de temps avec Arduino](https://learn.adafruit.com/ds1307-real-time-clock-breakout-board-kit?view=all)
