---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:08:52.616688-07:00
description: "Comment faire : Arduino lui-m\xEAme n'a pas de m\xE9thode int\xE9gr\xE9\
  e pour r\xE9cup\xE9rer directement la date actuelle, car il manque d'une horloge\
  \ en temps r\xE9el (RTC).\u2026"
lastmod: '2024-03-13T22:44:58.124828-06:00'
model: gpt-4-0125-preview
summary: "Arduino lui-m\xEAme n'a pas de m\xE9thode int\xE9gr\xE9e pour r\xE9cup\xE9\
  rer directement la date actuelle, car il manque d'une horloge en temps r\xE9el (RTC)."
title: Obtenir la date actuelle
weight: 29
---

## Comment faire :
Arduino lui-même n'a pas de méthode intégrée pour récupérer directement la date actuelle, car il manque d'une horloge en temps réel (RTC). Cependant, cela peut être réalisé en utilisant des modules RTC externes comme le DS3231, et des bibliothèques telles que `RTClib`, développées par Adafruit, qui rendent l'interface avec ces modules simple.

D'abord, assurez-vous que la bibliothèque `RTClib` est installée dans votre IDE Arduino. Ensuite, connectez votre module RTC à votre Arduino conformément à sa documentation.

Voici un exemple simple pour commencer :

```cpp
#include <Wire.h>
#include "RTClib.h"

RTC_DS3231 rtc;

void setup() {
  Serial.begin(9600);

  if (!rtc.begin()) {
    Serial.println("Couldn't find RTC");
    while (1);
  }

  if (rtc.lostPower()) {
    Serial.println("RTC a perdu de l'alimentation, mettons l'heure !");
    // Quand le temps doit être réglé sur un nouvel appareil ou après une perte de puissance, vous pouvez le régler ici.
    // rtc.adjust(DateTime(F(__DATE__), F(__TIME__)));
  }
}

void loop() {
  DateTime now = rtc.now();

  Serial.print("Date actuelle : ");
  Serial.print(now.year(), DEC);
  Serial.print('/');
  Serial.print(now.month(), DEC);
  Serial.print('/');
  Serial.println(now.day(), DEC);

  delay(3000); // Délai de 3 secondes pour réduire le spam série
}
```

Exemple de sortie (en supposant que votre RTC a été préalablement réglé) :

```
Date actuelle : 2023/4/15
```

Ce code initialise le module RTC puis, dans la boucle, récupère et imprime la date actuelle sur le moniteur série toutes les 3 secondes. Rappelez-vous, la ligne `rtc.adjust(...)` peut être décommentée et modifiée pour régler initialement la date et l'heure du RTC ou après qu'il ait perdu de la puissance.
