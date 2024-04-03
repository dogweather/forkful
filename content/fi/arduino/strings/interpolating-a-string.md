---
date: 2024-01-20 17:49:59.839884-07:00
description: "How to: (Kuinka tehd\xE4:) Arduino ei suoraan tue stringien interpolointia,\
  \ mutta voit saavuttaa saman vaikutuksen `sprintf`-funktiolla tai liitt\xE4m\xE4\
  ll\xE4\u2026"
lastmod: '2024-03-13T22:44:56.811306-06:00'
model: gpt-4-1106-preview
summary: "Arduino ei suoraan tue stringien interpolointia, mutta voit saavuttaa saman\
  \ vaikutuksen `sprintf`-funktiolla tai liitt\xE4m\xE4ll\xE4 merkkijonoja yhteen."
title: Merkkijonon interpolointi
weight: 8
---

## How to: (Kuinka tehdä:)
Arduino ei suoraan tue stringien interpolointia, mutta voit saavuttaa saman vaikutuksen `sprintf`-funktiolla tai liittämällä merkkijonoja yhteen.

```Arduino
char buffer[50];
int sensorValue = analogRead(A0);
sprintf(buffer, "Sensorin arvo: %d", sensorValue);
Serial.println(buffer);
```

Esimerkin tuloste:
```
Sensorin arvo: 345
```

## Deep Dive (Syväsukellus)
Stringien interpolointi on yleinen ominaisuus monissa moderneissa kielissä, kuten Python tai JavaScript. Arduino C++:ssa käytetään toimintoja, kuten `printf` tai `sprintf`, joissa yhdistelmä merkki `%` ja kirjain määrittelee muuttujan tyypin. Vaihtoehtoisesti voit käyttää `String`-luokkaa ja `+` operaattoria yksinkertaiseen yhdistämiseen, mutta dynaamisten `String`-olioiden jatkuva luominen voi johtaa muistiongelmiin, joten `sprintf` on usein suositumpi vaihtoehto tehopiirien kanssa työskenneltäessä.

## See Also (Katso Myös)
- Arduino `sprintf` reference: https://www.cplusplus.com/reference/cstdio/sprintf/
- Arduino `String` class reference: https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/
- Memory management with Arduino: https://learn.arduino.cc/learning-paths/ways-into-arduino/memory-management
