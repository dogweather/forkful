---
title:                "Trabajando con números complejos"
date:                  2024-03-08T21:57:37.673029-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Los números complejos, compuestos por una parte real y una parte imaginaria (generalmente denotados como a + bi), extienden el concepto de los números sin dimensión a un espacio bidimensional. Los programadores trabajan con números complejos en campos como la ingeniería eléctrica, la computación cuántica y la dinámica de fluidos para modelar fenómenos que no pueden representarse solo en una dimensión de números reales.

## Cómo hacerlo:

Dart no incluye una biblioteca incorporada para números complejos, lo que hace necesario implementar una clase de número complejo personalizada o usar una biblioteca de terceros. Una opción popular para tareas de computación científica, que incluye soporte para números complejos, es `package:scidart`.

### Implementando una Clase Básica de Números Complejos

Para operaciones simples, puedes definir fácilmente tu propia clase de número complejo:

```dart
class Complex {
  final double real;
  final double imaginary;

  Complex(this.real, this.imaginary);

  // Adición de dos números complejos
  Complex operator +(Complex other) {
    return Complex(real + other.real, imaginary + other.imaginary);
  }

  // Representación en cadena para depuración fácil
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

### Usando SciDart para Operaciones Avanzadas

Para operaciones más complejas o cuando el rendimiento es crítico, el `package:scidart` ofrece soporte comprensivo para números complejos entre otras funcionalidades de computación científica. Primero, añade SciDart a tu pubspec.yaml:

```yaml
dependencies:
  scidart: ^0.0.1-dev.9
```

Así es como se realizan operaciones básicas con números complejos usando SciDart:

```dart
import 'package:scidart/numdart.dart';

void main() {
  // Creando números complejos
  var complexNum1 = Complex(real: 5, imaginary: 3);
  var complexNum2 = Complex(real: 2, imaginary: 7);

  // Adición
  var sum = complexAdd(complexNum1, complexNum2);
  
  // Multiplicación
  var product = complexMultiply(complexNum1, complexNum2);

  print('Suma: ${sum.toString()}');  // Suma: Complex(real: 7.0, imaginary: 10.0)
  print('Producto: ${product.toString()}');  // Producto: Complex(real: -11.0, imaginary: 41.0)
}
```

Estos ejemplos demuestran la manipulación básica y la utilización de números complejos en Dart, tanto a través de la implementación personalizada como mediante la biblioteca SciDart, destacando la flexibilidad y la potencia de Dart para tareas de computación científica.
