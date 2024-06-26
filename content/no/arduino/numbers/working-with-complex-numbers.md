---
date: 2024-01-26 04:36:47.038685-07:00
description: "Hvordan: Opprinnelig ble komplekse tall m\xF8tt med skepsis, men de\
  \ har blitt sentrale i ulike vitenskapelige felt. Historisk sett ble de anerkjent\
  \ for \xE5 gi\u2026"
lastmod: '2024-04-05T22:50:55.058563-06:00'
model: gpt-4-0125-preview
summary: "Opprinnelig ble komplekse tall m\xF8tt med skepsis, men de har blitt sentrale\
  \ i ulike vitenskapelige felt."
title: "\xC5 jobbe med komplekse tall"
weight: 14
---

## Hvordan:
```Arduino
#include <Complex.h>

void setup() {
  Serial.begin(9600); // Start seriekommunikasjon
  
  Complex myComplex(2, 3); // Opprett et komplekst tall 2 + 3i
  Complex anotherComplex(1, 1); // Opprett et annet komplekst tall 1 + 1i
  
  // Addisjon
  Complex result = myComplex + anotherComplex; 
  Serial.print("Addisjon: "); 
  result.print(); // Gir ut 3 + 4i
  
  // Multiplikasjon
  result = myComplex * anotherComplex; 
  Serial.print("Multiplikasjon: ");
  result.print(); // Gir ut -1 + 5i
}

void loop() {
  // Ikke brukt i dette eksempelet
}
```
Eksempel på output:
```
Addisjon: 3 + 4i
Multiplikasjon: -1 + 5i
```

## Dypdykk
Opprinnelig ble komplekse tall møtt med skepsis, men de har blitt sentrale i ulike vitenskapelige felt. Historisk sett ble de anerkjent for å gi løsninger på polynomligninger som mangler reelle løsninger.

Arduino inkluderer ikke komplekse tall i sitt standardbibliotek, men du kan benytte biblioteker som `Complex.h` for å håndtere dem. Internt definerer disse bibliotekene en Complex-klasse, vanligvis ved å bruke to dobler for å lagre den reelle og den imaginære delen, og overbelaste operatorer for å støtte aritmetikk.

Som et alternativ, for applikasjoner som ikke inherent trenger aritmetikk med komplekse tall, vurder å bruke andre matte-strategier eller biblioteker. Husk, dog, at bruk av flyttall i stedet for komplekse tall kan forenkle noen problemer for mye.

## Se også
- [Complex.h](https://github.com/RobTillaart/Complex) biblioteket av Rob Tillaart.
- Et dypere dykk inn i [matematikken bak komplekse tall](https://mathworld.wolfram.com/ComplexNumber.html).
