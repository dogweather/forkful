---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:11.013616-07:00
description: "Getallen afronden is het proces van het aanpassen van een getal naar\
  \ het dichtstbijzijnde hele getal of naar een gespecificeerd aantal decimalen.\u2026"
lastmod: '2024-03-11T00:14:24.315492-06:00'
model: gpt-4-0125-preview
summary: "Getallen afronden is het proces van het aanpassen van een getal naar het\
  \ dichtstbijzijnde hele getal of naar een gespecificeerd aantal decimalen.\u2026"
title: Afronden van getallen
---

{{< edit_this_page >}}

## Wat & Waarom?

Getallen afronden is het proces van het aanpassen van een getal naar het dichtstbijzijnde hele getal of naar een gespecificeerd aantal decimalen. Programmeurs ronden vaak getallen af om berekeningen te vereenvoudigen, de leesbaarheid te verbeteren of data voor te bereiden voor weergave, wat zorgt voor consistentie en duidelijkheid in numerieke uitvoer.

## Hoe te:

Dart biedt inheemse methoden in zijn kern `num` type voor afrondingsbewerkingen. Hier zullen we methoden zoals `round()`, `floor()`, `ceil()`, en hoe af te ronden naar een specifiek aantal decimalen verkennen.

### Afronden naar het dichtstbijzijnde hele getal:

```dart
var number = 3.56;
print(number.round()); // Uitvoer: 4
```

### Naar beneden afronden:

```dart
print(number.floor()); // Uitvoer: 3
```

### Naar boven afronden:

```dart
print(number.ceil()); // Uitvoer: 4
```

### Afronden naar een specifiek aantal decimalen:

Om af te ronden naar een specifiek aantal decimalen, kunnen we de methode `toStringAsFixed()` gebruiken, die een string retourneert, of een combinatie van `pow` uit `dart:math` voor een numeriek resultaat.

```dart
import 'dart:math';

var number = 3.56789;
String roundedString = number.toStringAsFixed(2); // Voor weergavedoeleinden
print(roundedString); // Uitvoer: 3.57

double roundedNumber = double.parse(roundedString);
print(roundedNumber); // Uitvoer: 3.57

// Als alternatief, voor een numeriek resultaat:
double roundedToDecimal = (number * pow(10, 2)).round().toDouble() / pow(10, 2);
print(roundedToDecimal); // Uitvoer: 3.57
```

Hoewel de standaardbibliotheek van Dart de meeste afrondingsbehoeften effectief dekt, kunnen voor complexere wiskundige bewerkingen of precieze afrondingsvereisten bibliotheken zoals `decimal` nuttig zijn. De `decimal` bibliotheek biedt een gemakkelijke manier om met decimale getallen te werken zonder precisie te verliezen, wat vooral handig is voor financiële berekeningen, maar voor eenvoudige afrondingsmethoden zoals getoond, is de kernfunctionaliteit van Dart doorgaans voldoende.
