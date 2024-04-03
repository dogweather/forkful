---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:02:01.075148-07:00
description: "String interpolatie betreft het mengen van variabelen met tekst. Programmeurs\
  \ doen dit om strings on-the-fly te bouwen, waardoor de output dynamisch en\u2026"
lastmod: '2024-03-13T22:44:51.058755-06:00'
model: gpt-4-0125-preview
summary: String interpolatie betreft het mengen van variabelen met tekst.
title: Een string interpoleren
weight: 8
---

## Hoe te:
Arduino heeft geen ingebouwde stringinterpolatie, maar je kunt vergelijkbare resultaten bereiken met `sprintf()` of door strings en variabelen te concatenaten.

```Arduino
char buffer[50]; // Zorg ervoor dat dit groot genoeg is om de uiteindelijke string te bevatten
int sensorValue = analogRead(A0);
sprintf(buffer, "Sensorwaarde: %d", sensorValue);
Serial.println(buffer);
```

Output:
```
Sensorwaarde: 402
```

Of door het gebruik van stringconcatenatie:

```Arduino
String bericht = "Sensorwaarde: " + String(sensorValue);
Serial.println(bericht);
```

## Diepgaande duik
C en C++ (de kern talen van Arduino-sketches) hebben traditioneel geen stringinterpolatie zoals nieuwere talen (bijv., Python of JavaScript). In plaats daarvan is `sprintf()` lange tijd de favoriete manier geweest om strings met variabelen samen te stellen. Het werkt, maar het kan een beetje onhandig zijn en gevoelig voor fouten door bufferoverlopen als het niet zorgvuldig wordt beheerd.

Concatenatie met behulp van de `String` klasse is intuïtiever en veiliger tegen geheugenfouten. Het nadeel? Het kan leiden tot geheugenfragmentatie, vooral in langlopende programma's op geheugenbeperkte apparaten zoals Arduinos.

Een alternatief, dat in sommige nieuwere of meer gespecialiseerde C++ bibliotheken te vinden is (niet standaard in Arduino), is het gebruik van stringformatting bibliotheken die een syntax bieden die dichter bij interpolatie ligt, zoals `fmtlib`.

Wat betreft de implementatiedetails, wanneer je concateneert met de `String` klasse, creëert de Arduino achter de schermen nieuwe stringobjecten en beheert het geheugen voor je. `sprintf()`, aan de andere kant, schrijft geformatteerde tekst naar een buffer die je toewijst, waardoor je meer controle krijgt ten koste van het handmatig moeten beheren van geheugen.

## Zie ook
- Arduino `String` klasse referentie: https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/
- `sprintf()` functie referentie: http://www.cplusplus.com/reference/cstdio/sprintf/
- Arduino-geheugenoptimalisatie: https://www.arduino.cc/en/Tutorial/Foundations/Memory
- fmtlib, een moderne stringformatting bibliotheek: https://fmt.dev/latest/index.html
