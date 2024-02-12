---
title:                "Werken met complexe getallen"
aliases: - /nl/arduino/working-with-complex-numbers.md
date:                  2024-01-28T22:11:53.634124-07:00
model:                 gpt-4-0125-preview
simple_title:         "Werken met complexe getallen"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/arduino/working-with-complex-numbers.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Complexe getallen hebben een reëel deel en een imaginair deel, meestal geschreven als `a + bi`. Ze zijn essentieel voor sommige wiskunde-intensieve Arduino-projecten die te maken hebben met signaalbewerking, elektrotechniek, of elk ander domein waar verschijnselen het beste gemodelleerd worden in een vlak.

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
