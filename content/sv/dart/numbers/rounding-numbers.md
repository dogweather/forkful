---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:31.750319-07:00
description: "Hur: Dart tillhandah\xE5ller inf\xF6dda metoder i sin k\xE4rn `num`-typ\
  \ f\xF6r avrundningsoperationer. H\xE4r ska vi utforska metoder som `round()`, `floor()`,\
  \ `ceil()`,\u2026"
lastmod: '2024-03-13T22:44:37.606377-06:00'
model: gpt-4-0125-preview
summary: "Dart tillhandah\xE5ller inf\xF6dda metoder i sin k\xE4rn `num`-typ f\xF6\
  r avrundningsoperationer."
title: Avrundning av nummer
weight: 13
---

## Hur:
Dart tillhandahåller infödda metoder i sin kärn `num`-typ för avrundningsoperationer. Här ska vi utforska metoder som `round()`, `floor()`, `ceil()`, och hur man avrundar till ett specifikt antal decimaler.

### Avrunda till närmaste hela tal:
```dart
var number = 3.56;
print(number.round()); // Utskrift: 4
```

### Avrunda nedåt:
```dart
print(number.floor()); // Utskrift: 3
```

### Avrunda uppåt:
```dart
print(number.ceil()); // Utskrift: 4
```

### Avrunda till ett specifikt antal decimaler:
För att avrunda till ett specifikt antal decimaler kan vi använda metoden `toStringAsFixed()`, som returnerar en sträng, eller använda en kombination av `pow` från `dart:math` för ett numeriskt resultat.

```dart
import 'dart:math';

var number = 3.56789;
String rundadStrang = number.toStringAsFixed(2); // För visningssyften
print(rundadStrang); // Utskrift: 3.57

double rundatTal = double.parse(rundadStrang);
print(rundatTal); // Utskrift: 3.57

// Alternativt, för ett numeriskt resultat:
double avrundatTillDecimal = (number * pow(10, 2)).round().toDouble() / pow(10, 2);
print(avrundatTillDecimal); // Utskrift: 3.57
```

Medan Darts kärnbibliotek effektivt täcker de flesta behov av avrundning, kan bibliotek som `decimal` vara användbara för mer komplexa matematiska operationer eller precisa avrundningskrav. `Decimal`-biblioteket ger ett enkelt sätt att arbeta med decimaltal utan att förlora precision, vilket är särskilt praktiskt för finansiella beräkningar, men för enkla avrundningsmetoder som visas, är Darts kärnfunktionalitet oftast tillräcklig.
