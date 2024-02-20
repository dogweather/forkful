---
date: 2024-01-20 17:46:42.154501-07:00
description: "\xC5 finne lengden p\xE5 en string betyr \xE5 telle antall tegn i den.\
  \ Programmerere trenger det for \xE5 manipulere tekstdata presist, som for eksempel\
  \ \xE5 sjekke\u2026"
lastmod: 2024-02-19 22:05:00.320674
model: gpt-4-1106-preview
summary: "\xC5 finne lengden p\xE5 en string betyr \xE5 telle antall tegn i den. Programmerere\
  \ trenger det for \xE5 manipulere tekstdata presist, som for eksempel \xE5 sjekke\u2026"
title: "Finn lengden p\xE5 en streng"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å finne lengden på en string betyr å telle antall tegn i den. Programmerere trenger det for å manipulere tekstdata presist, som for eksempel å sjekke passordstyrke eller formatere utdata.

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
