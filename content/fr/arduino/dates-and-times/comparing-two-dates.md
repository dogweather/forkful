---
date: 2024-01-20 17:32:28.515744-07:00
description: "Comparer deux dates, c'est mesurer la diff\xE9rence entre elles ; on\
  \ le fait pour suivre des \xE9v\xE9nements, des d\xE9lais ou pour de la synchronisation.\
  \ C'est\u2026"
lastmod: '2024-03-11T00:14:32.027949-06:00'
model: gpt-4-1106-preview
summary: "Comparer deux dates, c'est mesurer la diff\xE9rence entre elles ; on le\
  \ fait pour suivre des \xE9v\xE9nements, des d\xE9lais ou pour de la synchronisation.\
  \ C'est\u2026"
title: Comparer deux dates
---

{{< edit_this_page >}}

## What & Why? (Quoi et pourquoi ?)
Comparer deux dates, c'est mesurer la différence entre elles ; on le fait pour suivre des événements, des délais ou pour de la synchronisation. C'est basique, mais super utile.

## How to: (Comment faire : )
Imagine t'as deux dates : début et fin. Tu veux savoir combien de temps s'est écoulé entre elles. On va utiliser les `DateTime` de la bibliothèque `RTClib`. Faut d'abord l'installer via le Gestionnaire de bibliothèques de l'EDI Arduino.

```Arduino
#include <RTClib.h>

RTC_DS3231 rtc;

void setup() {
  Serial.begin(9600);

  DateTime startDate = rtc.now(); // Supposons que ça, c'est ton début
  delay(10000); // Attends 10 secondes. Dans la vraie vie, ça pourrait être des jours
  DateTime endDate = rtc.now(); // Et là, c'est ta fin
  
  Serial.println("Début : " + startDate.toString());
  Serial.println("Fin : " + endDate.toString());
  
  TimeSpan duration = endDate - startDate;
  Serial.print("Durée : ");
  Serial.print(duration.days(), DEC);
  Serial.print(" jours, ");
  Serial.print(duration.hours(), DEC);
  Serial.print(" heures, ");
  Serial.print(duration.minutes(), DEC);
  Serial.print(" minutes, et ");
  Serial.print(duration.seconds(), DEC);
  Serial.println(" secondes.");
}

void loop() {
  // Ce qu’on met ici dépend de ton programme.
}
```

Sample output (Exemple de sortie) :
```
Début : 2023-03-25T15:46:57
Fin : 2023-03-25T15:47:07
Durée : 0 jours, 0 heures, 0 minutes, et 10 secondes.
```

## Deep Dive (Plongée en profondeur)
Comparer deux dates, c'est un concept ancestral en programmation. Avant `RTClib`, on jonglait avec des `timestamp` et des calculs manuels. Par rapport aux autres plateformes, Arduino nécessite un module RTC externe pour la précision horaire. Sinon, `millis()` peut aider pour des durées courtes.

Les alternatives incluent des bibliothèques comme `TimeLib.h`; certains vont préférer des calculs manuels pour des raisons de mémoire ou de contrôle. Pourtant, l'implémentation avec `RTClib` reste plus élégante et plus fiable pour des applications temporelles.

## See Also (Voir aussi)
Pour aller plus loin, voici des ressources :

- Documentation `RTClib`: https://github.com/adafruit/RTClib
- Arduino Time library: https://www.arduino.cc/en/Reference/Time
- Guide sur les modules RTC pour Arduino: https://lastminuteengineers.com/ds3231-rtc-arduino-tutorial/
