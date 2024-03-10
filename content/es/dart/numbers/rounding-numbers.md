---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:35.055946-07:00
description: "Redondear n\xFAmeros es el proceso de ajustar un n\xFAmero a su n\xFA\
  mero entero m\xE1s cercano o a un n\xFAmero especificado de decimales. Los programadores\
  \ a menudo\u2026"
lastmod: '2024-03-09T21:06:22.322455-07:00'
model: gpt-4-0125-preview
summary: "Redondear n\xFAmeros es el proceso de ajustar un n\xFAmero a su n\xFAmero\
  \ entero m\xE1s cercano o a un n\xFAmero especificado de decimales. Los programadores\
  \ a menudo\u2026"
title: "Redondeo de n\xFAmeros"
---

{{< edit_this_page >}}

## Qué y Por Qué?

Redondear números es el proceso de ajustar un número a su número entero más cercano o a un número especificado de decimales. Los programadores a menudo redondean números para simplificar cálculos, mejorar la legibilidad o preparar datos para su visualización, asegurando consistencia y claridad en las salidas numéricas.

## Cómo hacerlo:

Dart proporciona métodos nativos en su tipo central `num` para operaciones de redondeo. Aquí, exploraremos métodos como `round()`, `floor()`, `ceil()`, y cómo redondear a un número específico de decimales.

### Redondeando al entero más cercano:

```dart
var number = 3.56;
print(number.round()); // Salida: 4
```

### Redondeando hacia abajo:

```dart
print(number.floor()); // Salida: 3
```

### Redondeando hacia arriba:

```dart
print(number.ceil()); // Salida: 4
```

### Redondeando a un número específico de decimales:

Para redondear a un número específico de decimales, podemos usar el método `toStringAsFixed()`, que devuelve una cadena, o usar una combinación de `pow` de `dart:math` para obtener un resultado numérico.

```dart
import 'dart:math';

var number = 3.56789;
String redondeadoString = number.toStringAsFixed(2); // Para propósitos de visualización
print(redondeadoString); // Salida: 3.57

double numeroRedondeado = double.parse(redondeadoString);
print(numeroRedondeado); // Salida: 3.57

// Alternativamente, para un resultado numérico:
double redondeadoADecimal = (number * pow(10, 2)).round().toDouble() / pow(10, 2);
print(redondeadoADecimal); // Salida: 3.57
```

Aunque la biblioteca central de Dart cubre la mayoría de las necesidades de redondeo de manera efectiva, para operaciones matemáticas más complejas o requisitos de redondeo precisos, bibliotecas como `decimal` pueden ser útiles. La biblioteca `decimal` proporciona una forma fácil de trabajar con números decimales sin perder precisión, lo que es especialmente práctico para cálculos financieros, pero para métodos de redondeo simples como se muestra, la funcionalidad central de Dart es generalmente suficiente.
