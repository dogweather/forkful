---
date: 2024-01-20 17:46:42.154501-07:00
description: "Hvordan gj\xF8res det: I tidlige dager av computing, var strenglengde\
  \ viktig for minneh\xE5ndtering. Arduino bruker `String`-objektet som innkapsler\
  \ mange\u2026"
lastmod: '2024-04-05T22:50:55.055095-06:00'
model: gpt-4-1106-preview
summary: "I tidlige dager av computing, var strenglengde viktig for minneh\xE5ndtering."
title: "Finn lengden p\xE5 en streng"
weight: 7
---

## Hvordan gjøres det:
```Arduino
String tekst = "Hei, Norge!";
int lengde = tekst.length();

Serial.begin(9600);
Serial.print("Lengden på strengen er: ");
Serial.println(lengde);
```
Utdata:
```
Lengden på strengen er: 11
```

## Dypdykk:
I tidlige dager av computing, var strenglengde viktig for minnehåndtering. Arduino bruker `String`-objektet som innkapsler mange funksjoner, `length()` er en av de. Alternativt kan du bruke C-stil strenger (char arrays) og `strlen()` funksjonen for å oppnå samme resultat. `String.length()` i Arduino gir en direkte måte å finne lengden uten å tenke på null-terminering, som er påkrevd med `char` arrays.

## Se også:
- Arduino sin offisielle `String` referanse: https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/
- Cplusplus.com sin guide til `strlen()`: http://www.cplusplus.com/reference/cstring/strlen/
