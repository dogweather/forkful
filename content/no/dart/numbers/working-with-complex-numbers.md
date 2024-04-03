---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:58:00.674315-07:00
description: "Hvordan: Dart selv inkluderer ikke et innebygd bibliotek for komplekse\
  \ tall, noe som n\xF8dvendiggj\xF8r enten implementeringen av en egen klasse for\
  \ komplekse\u2026"
lastmod: '2024-03-13T22:44:40.480578-06:00'
model: gpt-4-0125-preview
summary: "Dart selv inkluderer ikke et innebygd bibliotek for komplekse tall, noe\
  \ som n\xF8dvendiggj\xF8r enten implementeringen av en egen klasse for komplekse\
  \ tall eller bruk av et tredjepartsbibliotek."
title: Arbeide med komplekse tall
weight: 14
---

## Hvordan:
Dart selv inkluderer ikke et innebygd bibliotek for komplekse tall, noe som nødvendiggjør enten implementeringen av en egen klasse for komplekse tall eller bruk av et tredjepartsbibliotek. Et populært valg for vitenskapelig databehandlingsoppgaver, som inkluderer støtte for komplekse tall, er `package:scidart`.

### Implementering av en Grunnleggende Klasse for Komplekse Tall
For enkle operasjoner kan du enkelt definere din egen klasse for komplekse tall:

```dart
class Complex {
  final double real;
  final double imaginary;

  Complex(this.real, this.imaginary);

  // Addisjon av to komplekse tall
  Complex operator +(Complex other) {
    return Complex(real + other.real, imaginary + other.imaginary);
  }

  // Strengrepresentasjon for enkel feilsøking
  @override
  String toString() => '${real} + ${imaginary}i';
}

void main() {
  var number1 = Complex(3, 4);
  var number2 = Complex(1, 2);

  var sum = number1 + number2;
  print(sum);  // 4.0 + 6.0i
}
```

### Bruk av SciDart for Avanserte Operasjoner
For mer komplekse operasjoner eller når ytelse er kritisk, tilbyr `package:scidart` omfattende støtte for komplekse tall blant annet vitenskapelig databehandlingsfunksjonaliteter. Først, legg til SciDart i din pubspec.yaml:

```yaml
dependencies:
  scidart: ^0.0.1-dev.9
```

Slik utfører du grunnleggende operasjoner med komplekse tall ved hjelp av SciDart:

```dart
import 'package:scidart/numdart.dart';

void main() {
  // Oppretting av komplekse tall
  var complexNum1 = Complex(real: 5, imaginary: 3);
  var complexNum2 = Complex(real: 2, imaginary: 7);

  // Addisjon
  var sum = complexAdd(complexNum1, complexNum2);
  
  // Multiplikasjon
  var product = complexMultiply(complexNum1, complexNum2);

  print('Sum: ${sum.toString()}');  // Sum: Complex(real: 7.0, imaginary: 10.0)
  print('Product: ${product.toString()}');  // Produkt: Complex(real: -11.0, imaginary: 41.0)
}
```

Disse eksemplene viser grunnleggende manipulering og bruk av komplekse tall i Dart, både gjennom tilpasset implementering og via SciDart-biblioteket, og fremhever fleksibiliteten og kraften til Dart for vitenskapelige databehandlingsoppgaver.
