---
title:                "Obtenir la date actuelle"
date:                  2024-01-20T15:12:55.830001-07:00
html_title:           "C: Obtenir la date actuelle"
simple_title:         "Obtenir la date actuelle"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why?
Obtenir la date actuelle, c'est lire la valeur temporelle du moment présent. Les programmeurs le font pour enregistrer des événements, programmer des actions ou créer des journaux de débogage.

## How to:
Pour avoir la date sur Arduino, utilisez un module RTC (Real Time Clock) comme le DS3231. Voici comment ça se passe:

```Arduino
#include <Wire.h>
#include <RTClib.h>

RTC_DS3231 rtc;

void setup() {
  Serial.begin(9600);
  if (!rtc.begin()) {
    Serial.println("Impossible de trouver RTC");
    while (1);
  }
  if (rtc.lostPower()) {
    Serial.println("RTC perdu puissance, definir l'heure!");
    // Ligne ci-dessous à utiliser si l'horloge a perdu l'heure, après la première configuration.
    // rtc.adjust(DateTime(F(__DATE__), F(__TIME__)));
  }
}

void loop() {
  DateTime now = rtc.now();
  
  Serial.print(now.year());
  Serial.print('/');
  Serial.print(now.month());
  Serial.print('/');
  Serial.println(now.day());
  
  delay(1000);
}
```

Sample output:
```
2023/4/10
```

## Deep Dive
L'historique d'Arduino et des RTC remonte à des horloges externes en raison de l'absence d'horloge interne dans la plupart des microcontrôleurs Arduino. Avant les RTC, les gens utilisaient les temporisateurs ou des services de temps internet pour une date approximative.

Le DS3231 est apprécié pour sa précision. Alternatives? DS1307 (moins cher, moins précis) ou utilisez un module WiFi/Bluetooth pour synchroniser avec un serveur de temps.

Niveau implémentation, le DS3231 communique via I2C. Installation `RTClib` facile via le gestionnaire de bibliothèque de l'IDE Arduino. Une fois initialisé, un objet `DateTime` offre l'accès aux composants de la date.

## See Also
- `RTClib` library sur GitHub : [https://github.com/adafruit/RTClib](https://github.com/adafruit/RTClib)
- Documentation de `Wire` (pour I2C) : [https://www.arduino.cc/en/Reference/Wire](https://www.arduino.cc/en/Reference/Wire)
- Arduino Time Library pour des tâches temporelles avancées : [https://www.arduino.cc/playground/Code/Time](https://www.arduino.cc/playground/Code/Time)
- Module DS3231 sur Adafruit : [https://learn.adafruit.com/adafruit-ds3231-precision-rtc-breakout](https://learn.adafruit.com/adafruit-ds3231-precision-rtc-breakout)