---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:59:57.357203-07:00
description: "Hoe: Vroeger gebruikten C-programmeurs de `strlen()` functie uit `<string.h>`,\
  \ tellend tot een null-terminator. In de wereld van Arduino maakt de\u2026"
lastmod: '2024-04-05T22:51:03.863230-06:00'
model: gpt-4-0125-preview
summary: Vroeger gebruikten C-programmeurs de `strlen()` functie uit `<string.h>`,
  tellend tot een null-terminator.
title: De lengte van een string vinden
weight: 7
---

## Hoe:
```Arduino
void setup() {
  Serial.begin(9600); // Start de seriële communicatie
  String myString = "Hallo, Arduino!"; // Jouw string hier
  int stringLength = myString.length(); // De lengte van de string vinden
  Serial.print("De lengte van de string is: ");
  Serial.println(stringLength); // Geeft de lengte weer
}

void loop() {
  // Niets te doen hier.
}
```
Voorbeelduitvoer:
```
De lengte van de string is: 15
```

## Diepere duik
Vroeger gebruikten C-programmeurs de `strlen()` functie uit `<string.h>`, tellend tot een null-terminator. In de wereld van Arduino maakt de `String`-klasse het leven makkelijker met zijn ingebouwde `length()`-methode. Maar vergeet niet, het gebruik van `String`-objecten kan na verloop van tijd het beperkte geheugen van je apparaat fragmenteren. Een alternatief? Gebruik char arrays (C-stijl strings), die vriendelijker zijn voor het geheugen maar lastiger te hanteren.

Voor grotere projecten, overweeg altijd het beheer van geheugen. Met de `length()`-methode is geen extra berekening nodig - het `String`-object houdt zelf de grootte bij. Intern is `length()` een snelle opzoeking, geen tekentelling. Dat is efficiënt! Maar als je weinig geheugen hebt, keer dan terug naar de basis met char arrays en handmatige lengteberekeningen, net zoals in de goede oude `strlen()` dagen.

## Zie ook
- Arduino `String` Referentie: https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/
- Arduino `strlen()` functie voor C-stijl strings: https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/strlen/
- Discussie over `String` vs. char array in Arduino: https://forum.arduino.cc/t/string-vs-char-array/678207
