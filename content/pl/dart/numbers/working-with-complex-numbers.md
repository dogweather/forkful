---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:57:46.897329-07:00
description: "Jak to zrobi\u0107: Sam Dart nie zawiera wbudowanej biblioteki dla liczb\
  \ zespolonych, co wymaga albo implementacji w\u0142asnej klasy liczby zespolonej,\
  \ albo u\u017Cycia\u2026"
lastmod: '2024-03-13T22:44:35.084390-06:00'
model: gpt-4-0125-preview
summary: "Sam Dart nie zawiera wbudowanej biblioteki dla liczb zespolonych, co wymaga\
  \ albo implementacji w\u0142asnej klasy liczby zespolonej, albo u\u017Cycia biblioteki\
  \ stron trzecich."
title: Praca z liczbami zespolonymi
weight: 14
---

## Jak to zrobić:
Sam Dart nie zawiera wbudowanej biblioteki dla liczb zespolonych, co wymaga albo implementacji własnej klasy liczby zespolonej, albo użycia biblioteki stron trzecich. Popularnym wyborem dla zadań związanych z obliczeniami naukowymi, który obejmuje wsparcie dla liczb zespolonych, jest `package:scidart`.

### Implementacja podstawowej klasy liczby zespolonej
Do prostych operacji łatwo można zdefiniować własną klasę liczby zespolonej:

```dart
class Complex {
  final double real;
  final double imaginary;

  Complex(this.real, this.imaginary);

  // Dodawanie dwóch liczb zespolonych
  Complex operator +(Complex other) {
    return Complex(real + other.real, imaginary + other.imaginary);
  }

  // Reprezentacja tekstowa dla łatwego debugowania
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

### Użycie SciDart do zaawansowanych operacji
Do bardziej złożonych operacji lub gdy wydajność jest krytyczna, `package:scidart` oferuje wszechstronne wsparcie dla liczb zespolonych wśród innych funkcjonalności obliczeń naukowych. Najpierw dodaj SciDart do swojego pliku pubspec.yaml:

```yaml
dependencies:
  scidart: ^0.0.1-dev.9
```

Oto jak wykonać podstawowe operacje z liczbami zespolonymi przy użyciu SciDart:

```dart
import 'package:scidart/numdart.dart';

void main() {
  // Tworzenie liczb zespolonych
  var complexNum1 = Complex(real: 5, imaginary: 3);
  var complexNum2 = Complex(real: 2, imaginary: 7);

  // Dodawanie
  var sum = complexAdd(complexNum1, complexNum2);
  
  // Mnożenie
  var product = complexMultiply(complexNum1, complexNum2);

  print('Suma: ${sum.toString()}');  // Suma: Complex(real: 7.0, imaginary: 10.0)
  print('Iloczyn: ${product.toString()}');  // Iloczyn: Complex(real: -11.0, imaginary: 41.0)
}
```

Te przykłady demonstrują podstawową manipulację i użytkowanie liczb zespolonych w Dart, zarówno poprzez własną implementację, jak i za pośrednictwem biblioteki SciDart, podkreślając elastyczność i moc Darta w zadaniach obliczeniowych związanych z nauką.
