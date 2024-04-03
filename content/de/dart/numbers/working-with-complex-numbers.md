---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:57:37.098847-07:00
description: "Komplexe Zahlen, bestehend aus einem Real- und einem Imagin\xE4rteil\
  \ (\xFCblicherweise als a + bi dargestellt), erweitern das Konzept der dimensionslosen\
  \ Zahlen\u2026"
lastmod: '2024-03-13T22:44:53.573965-06:00'
model: gpt-4-0125-preview
summary: "Komplexe Zahlen, bestehend aus einem Real- und einem Imagin\xE4rteil (\xFC\
  blicherweise als a + bi dargestellt), erweitern das Konzept der dimensionslosen\
  \ Zahlen auf einen zweidimensionalen Raum."
title: Arbeiten mit komplexen Zahlen
weight: 14
---

## Was & Warum?

Komplexe Zahlen, bestehend aus einem Real- und einem Imaginärteil (üblicherweise als a + bi dargestellt), erweitern das Konzept der dimensionslosen Zahlen auf einen zweidimensionalen Raum. Programmierer arbeiten in Bereichen wie der Elektrotechnik, der Quanteninformatik und der Strömungsdynamik mit komplexen Zahlen, um Phänomene zu modellieren, die nicht allein entlang einer einzigen Dimension von reellen Zahlen dargestellt werden können.

## Wie geht das:

Dart selbst beinhaltet keine integrierte Bibliothek für komplexe Zahlen, was entweder die Implementierung einer eigenen komplexen Zahlklasse oder die Verwendung einer Bibliothek von Drittanbietern erforderlich macht. Eine beliebte Wahl für wissenschaftliche Rechenaufgaben, die auch die Unterstützung für komplexe Zahlen umfasst, ist `package:scidart`.

### Implementierung einer grundlegenden komplexen Zahlklasse

Für einfache Operationen können Sie leicht Ihre eigene komplexe Zahlklasse definieren:

```dart
class Complex {
  final double real;
  final double imaginary;

  Complex(this.real, this.imaginary);

  // Addition von zwei komplexen Zahlen
  Complex operator +(Complex other) {
    return Complex(real + other.real, imaginary + other.imaginary);
  }

  // Stringdarstellung für einfaches Debugging
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

### SciDart für fortgeschrittene Operationen nutzen

Für komplexere Operationen oder wenn die Leistung kritisch ist, bietet das `package:scidart` umfassende Unterstützung für komplexe Zahlen unter anderem wissenschaftlichen Rechenfunktionen. Fügen Sie zunächst SciDart zu Ihrer pubspec.yaml hinzu:

```yaml
dependencies:
  scidart: ^0.0.1-dev.9
```

So führen Sie grundlegende Operationen mit komplexen Zahlen unter Verwendung von SciDart durch:

```dart
import 'package:scidart/numdart.dart';

void main() {
  // Erstellen von komplexen Zahlen
  var complexNum1 = Complex(real: 5, imaginary: 3);
  var complexNum2 = Complex(real: 2, imaginary: 7);

  // Addition
  var sum = complexAdd(complexNum1, complexNum2);
  
  // Multiplikation
  var product = complexMultiply(complexNum1, complexNum2);

  print('Sum: ${sum.toString()}');  // Summe: Complex(real: 7.0, imaginary: 10.0)
  print('Product: ${product.toString()}');  // Produkt: Complex(real: -11.0, imaginary: 41.0)
}
```

Diese Beispiele demonstrieren die grundlegende Manipulation und Nutzung komplexer Zahlen in Dart, sowohl durch benutzerdefinierte Implementierung als auch über die SciDart-Bibliothek und heben die Flexibilität und Leistungsfähigkeit von Dart für wissenschaftliche Rechenaufgaben hervor.
