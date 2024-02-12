---
title:                "Fouten afhandelen"
aliases:
- nl/arduino/handling-errors.md
date:                  2024-01-28T22:01:31.455171-07:00
model:                 gpt-4-0125-preview
simple_title:         "Fouten afhandelen"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/arduino/handling-errors.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Foutafhandeling in je programma's vangt de onvoorziene dingen op die je proberen te laten struikelen. Je doet het om te voorkomen dat je Arduino een meltdown krijgt wanneer het onverwachte gebeurt.

## Hoe:

Stel je voor dat je Arduino een sensor leest die mogelijk af en toe waarden buiten het bereik produceert. Hier is hoe je dat zou kunnen aanpakken:

```Arduino
int sensorWaarde = analogRead(A0);

if (sensorWaarde >= 0 && sensorWaarde <= 1023) {
  // Waarde is binnen het bereik, ga door met verwerken
  Serial.println(sensorWaarde);
} else {
  // Waarde is buiten het bereik, handel de fout af
  Serial.println("Fout: Sensorwaarde buiten bereik.");
}
```
Voorbeelduitvoer:
```
523
Fout: Sensorwaarde buiten bereik.
761
```

## Diepgaand

Foutafhandeling is niet altijd zo eenvoudig geweest. In de vroege dagen negeerden ontwikkelaars vaak fouten, wat leidde tot het gevreesde "onbepaald gedrag". Naarmate programmeren zich ontwikkelde, deden de hulpmiddelen dat ook - je hebt nu uitzonderingen in veel talen, maar door hardwarebeperkingen en C++-wortels is het in de Arduino-wereld nog steeds een ouderwetse 'controleer-het-eerst'.

Bij Arduino-programmering zie je vaak `if-else`-verklaringen voor foutafhandeling. Maar er zijn alternatieven: het gebruik van de `assert`-functie om de uitvoering te stoppen als een voorwaarde faalt of het ontwerpen van noodmaatregelen binnen je hardware-opstelling zelf.

Bij het implementeren van foutafhandeling, overweeg de impact van het stoppen van het programma versus het toestaan om door te gaan met een standaard of veilige status. Er is een afweging, en de juiste keuze hangt af van de potentiële schade van onderbrekingen versus incorrecte werking.

## Zie Ook

Verdiep je in foutdetectie en afhandeling met deze bronnen:

- Arduino Taalreferentie: https://www.arduino.cc/reference/en/
- De diepere blik op foutafhandeling van Embedded Artistry: https://embeddedartistry.com/blog/2017/05/17/creating-a-circular-buffer-in-c-and-c/
- C++ Foutafhandeling: https://en.cppreference.com/w/cpp/error/exception

Dit zou je de kennis en het vertrouwen moeten geven om de valkuilen van fouten in je Arduino-avonturen te vermijden.
