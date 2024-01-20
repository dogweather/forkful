---
title:                "Obtenir la date actuelle"
html_title:           "Bash: Obtenir la date actuelle"
simple_title:         "Obtenir la date actuelle"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Qu'est-ce & Pourquoi?

Obtenir la date actuelle consiste à récupérer l'information sur le moment présent dans le format Jour/Mois/Année. Les programmeurs le font pour gérer et suivre le temps dans leurs projets.

## Comment faire:

Voici un exemple de code pour obtenir la date actuelle sur Arduino. Assurez-vous d'avoir un shield RTC connecté à votre Arduino. 

```Arduino
#include <Wire.h>
#include "RTClib.h"

RTC_DS1307 rtc;

void setup () {
  Serial.begin(57600);
  Wire.begin();
  rtc.begin();

  if (! rtc.isrunning()) {
    Serial.println("RTC n'est pas en cours d'execution!");
    rtc.adjust(DateTime(F(__DATE__), F(__TIME__)));
  }
}

void loop () {
  DateTime now = rtc.now();
  
  Serial.print(now.year(), DEC);
  Serial.print('/');
  Serial.print(now.month(), DEC);
  Serial.print('/');
  Serial.println(now.day(), DEC);
}
```

Ce code affiche la date actuelle dans la console série à chaque fois que vous démarrez votre Arduino.

## Deep Dive

Historiquement, les fonctions de gestion du temps étaient déjà intégrées aux premiers systèmes de calcul analogiques. Aujourd'hui, avec la prolifération des appareils numériques, il est encore plus essentiel de suivre le temps de façon précise.

Il existe de nombreuses alternatives pour obtenir la date actuelle, comme utiliser un module GPS ou même se connecter à un serveur NTP pour obtenir la date et l'heure exactes. Chaque alternative a ses propres avantages et inconvénients, il faut donc choisir celle qui convient le mieux à votre projet.

Pour obtenir la date actuelle avec Arduino, vous pouvez utiliser la bibliothèque RTClib, qui offre des fonctions faciles à utiliser pour gérer le temps. Elle implémente une interface de communication avec la puce RTC DS1307 qui est largement utilisée dans de nombreux shields RTC pour Arduino. 

## Voir Aussi

Vous pouvez approfondir vos connaissances sur la gestion du temps dans Arduino en consultant ces sources:
- La documentation officielle du RTC DS1307: [DS1307](https://datasheets.maximintegrated.com/en/ds/DS1307.pdf)
- Guide d'introduction à la bibliothèque RTClib: [RTClib](https://github.com/adafruit/RTClib)