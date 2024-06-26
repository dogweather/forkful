---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:11:53.634124-07:00
description: "Hoe te: Oorspronkelijk werden complexe getallen met scepsis ontvangen,\
  \ maar ze zijn centraal komen te staan in diverse wetenschappelijke velden.\u2026"
lastmod: '2024-04-05T22:51:03.866432-06:00'
model: gpt-4-0125-preview
summary: Oorspronkelijk werden complexe getallen met scepsis ontvangen, maar ze zijn
  centraal komen te staan in diverse wetenschappelijke velden.
title: Werken met complexe getallen
weight: 14
---

## Hoe te:
```Arduino
#include <Complex.h>

void setup() {
  Serial.begin(9600); // Start seriële communicatie
  
  Complex mijnComplex(2, 3); // Maak een complex getal 2 + 3i
  Complex anderComplex(1, 1); // Maak een ander complex getal 1 + 1i
  
  // Optelling
  Complex resultaat = mijnComplex + anderComplex; 
  Serial.print("Optelling: "); 
  resultaat.print(); // Geeft uit 3 + 4i
  
  // Vermenigvuldiging
  resultaat = mijnComplex * anderComplex; 
  Serial.print("Vermenigvuldiging: ");
  resultaat.print(); // Geeft uit -1 + 5i
}

void loop() {
  // Niet gebruikt in dit voorbeeld
}
```
Voorbeelduitvoer:
```
Optelling: 3 + 4i
Vermenigvuldiging: -1 + 5i
```

## Diepere Duik
Oorspronkelijk werden complexe getallen met scepsis ontvangen, maar ze zijn centraal komen te staan in diverse wetenschappelijke velden. Historisch gezien werden ze erkend voor het bieden van oplossingen voor polynomiale vergelijkingen die geen reële oplossingen hebben.

Arduino bevat geen complexe getallen in zijn standaardbibliotheek, maar je kunt bibliotheken zoals `Complex.h` gebruiken om ermee om te gaan. Intern definiëren deze bibliotheken een Complex klasse, meestal met twee doubles om het reële en imaginaire deel op te slaan, en overschrijven operatoren om rekenkundige ondersteuning te bieden.

Als alternatief, voor toepassingen die niet inherent complexe getallenrekenkunde nodig hebben, overweeg andere wiskundige strategieën of bibliotheken te gebruiken. Onthoud echter dat het gebruik van floats in plaats van complexe getallen sommige problemen kan simplificeren.

## Zie Ook
- De [Complex.h](https://github.com/RobTillaart/Complex) bibliotheek van Rob Tillaart.
- Een diepere duik in de [wiskunde achter complexe getallen](https://mathworld.wolfram.com/ComplexNumber.html).
