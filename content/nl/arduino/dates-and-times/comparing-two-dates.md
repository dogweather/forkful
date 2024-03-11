---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:56:17.895644-07:00
description: "Twee datums vergelijken betekent uitzoeken welke eerder is, later, of\
  \ als ze hetzelfde zijn. Programmeurs doen dit om tijdgebaseerde evenementen bij\
  \ te\u2026"
lastmod: '2024-03-11T00:14:24.919304-06:00'
model: gpt-4-0125-preview
summary: "Twee datums vergelijken betekent uitzoeken welke eerder is, later, of als\
  \ ze hetzelfde zijn. Programmeurs doen dit om tijdgebaseerde evenementen bij te\u2026"
title: Twee data vergelijken
---

{{< edit_this_page >}}

## Wat & Waarom?
Twee datums vergelijken betekent uitzoeken welke eerder is, later, of als ze hetzelfde zijn. Programmeurs doen dit om tijdgebaseerde evenementen bij te houden, zoals het plannen van taken of het loggen van gegevens over tijd.

## Hoe:
In Arduino kun je datums vergelijken met behulp van de `TimeLib.h` bibliotheek. Installeer deze eerst. Bekijk vervolgens deze code:

```Arduino
#include <TimeLib.h>

void setup() {
  Serial.begin(9600);
  // Stel twee verschillende tijden in (jaar, maand, dag, uur, minuut, seconde)
  // Hier stellen we in 3 maart 2023, 8:30:00 en 4 maart 2023, 16:45:00
  time_t eersteTijd = makeTime({0, 30, 8, 3, 3, 2023});
  time_t tweedeTijd = makeTime({0, 45, 16, 4, 3, 2023});
  
  // Vergelijk de twee tijden
  if (eersteTijd < tweedeTijd) {
    Serial.print("Eerste tijd is eerder.");
  } else if (eersteTijd > tweedeTijd) {
    Serial.print("Tweede tijd is eerder.");
  } else {
    Serial.print("Beide tijden zijn hetzelfde.");
  }
}

void loop() {
  // Niets hier
}
```

Voorbeelduitvoer:
```
Eerste tijd is eerder.
```

## Diepere Duik
Arduino heeft geen ingebouwde ondersteuning voor datum en tijd, dus we gebruiken bibliotheken zoals `TimeLib.h`. Voordat er bibliotheken waren, moesten mensen handmatig datums berekenen en vergelijken - lastig vanwege schrikkeljaren, verschillende maandlengten, en dergelijke.

Andere manieren om met datums om te gaan zijn RTC (Real Time Clock) modules, zoals de DS3231, die de tijd bijhouden zelfs wanneer de Arduino uit staat. Voor het vergelijken zou je nog steeds de datums in je programma halen en ze vergelijken zoals we hierboven deden.

Neem bij de implementatie rekening met tijdzones en zomertijd indien nodig. TimeLib kan omgaan met UTC-tijd, wat deze kwesties omzeilt, maar lokale tijden vereisen extra zorg.

## Zie Ook
- [TimeLib Library Documentatie](https://www.pjrc.com/teensy/td_libs_Time.html) - Details over het gebruik van de Time bibliotheek.
- [Arduino Time Library](https://github.com/PaulStoffregen/Time) - De GitHub repository voor de Time bibliotheek.
