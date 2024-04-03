---
date: 2024-01-26 04:36:48.404629-07:00
description: 'Kuinka: .'
lastmod: '2024-03-13T22:44:56.819164-06:00'
model: gpt-4-0125-preview
summary: .
title: "Kompleksilukujen k\xE4sittely"
weight: 14
---

## Kuinka:
```Arduino
#include <Complex.h>

void setup() {
  Serial.begin(9600); // Aloita sarjaviestintä
  
  Complex myComplex(2, 3); // Luo kompleksiluku 2 + 3i
  Complex anotherComplex(1, 1); // Luo toinen kompleksiluku 1 + 1i
  
  // Yhteenlasku
  Complex result = myComplex + anotherComplex; 
  Serial.print("Yhteenlasku: "); 
  result.print(); // Tulostaa 3 + 4i
  
  // Kertolasku
  result = myComplex * anotherComplex; 
  Serial.print("Kertolasku: ");
  result.print(); // Tulostaa -1 + 5i
}

void loop() {
  // Ei käytössä tässä esimerkissä
}
```
Esimerkkituloste:
```
Yhteenlasku: 3 + 4i
Kertolasku: -1 + 5i
```

## Syväsukellus
Alun perin kompleksiluvut kohtasivat skeptisismiä, mutta ne ovat tulleet keskeisiksi monilla tieteellisillä aloilla. Historiallisesti ne tunnustettiin tarjoavan ratkaisuja polynomiyhtälöille, joilla ei ole reaalisia ratkaisuja.

Arduino ei sisällä kompleksilukuja sen vakio kirjastossa, mutta voit hyödyntää kirjastoja kuten `Complex.h` niiden käsittelyyn. Sisäisesti nämä kirjastot määrittelevät Complex-luokan, käyttäen yleensä kahta double-tyyppiä reaaliosan ja imaginääriosan tallentamiseen, ja ylikuormittavat operaattorit aritmeettista tukea varten.

Vaihtoehtoisesti, sovelluksille, jotka eivät sinänsä tarvitse kompleksilukujen aritmetiikkaa, harkitse muiden matematiikkastrategioiden tai kirjastojen käyttöä. Muista kuitenkin, että float-tyyppien käyttäminen kompleksilukujen sijaan saattaa yksinkertaistaa joitakin ongelmia liikaa.

## Katso Myös
- [Complex.h](https://github.com/RobTillaart/Complex) kirjasto Rob Tillaartin toimesta.
- Syvempi sukellus [kompleksilukujen matematiikkaan](https://mathworld.wolfram.com/ComplexNumber.html).
