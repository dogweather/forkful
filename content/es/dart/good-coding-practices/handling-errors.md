---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:30.516104-07:00
description: "Manejar errores en Dart se trata de anticipar y gestionar las excepciones\
  \ que surgen durante la ejecuci\xF3n del programa para mejorar la confiabilidad\
  \ y la\u2026"
lastmod: '2024-03-13T22:44:58.760534-06:00'
model: gpt-4-0125-preview
summary: "Manejar errores en Dart se trata de anticipar y gestionar las excepciones\
  \ que surgen durante la ejecuci\xF3n del programa para mejorar la confiabilidad\
  \ y la usabilidad."
title: Manejo de errores
weight: 16
---

## Qué y Por Qué
Manejar errores en Dart se trata de anticipar y gestionar las excepciones que surgen durante la ejecución del programa para mejorar la confiabilidad y la usabilidad. Los programadores implementan el manejo de errores para prevenir fallos y proporcionar retroalimentación significativa a los usuarios, asegurando una experiencia de aplicación más suave y segura.

## Cómo hacerlo:
Dart soporta dos tipos de errores: errores en *tiempo de compilación* y errores en *tiempo de ejecución*. Los errores en tiempo de compilación son detectados por el analizador de Dart antes de que el código se ejecute, mientras que los errores en tiempo de ejecución, o excepciones, ocurren durante la ejecución. Aquí está cómo manejar excepciones en Dart:

### Try-Catch
Usa `try-catch` para capturar excepciones y prevenir que hagan colapsar tu aplicación:

```dart
try {
  var result = 100 ~/ 0; // Intentando división por cero, lanza una excepción
} catch (e) {
  print('Capturada una excepción: $e'); // Maneja la excepción
}
```
Salida de muestra: `Capturada una excepción: IntegerDivisionByZeroException`

### Excepción Específica
Para manejar excepciones específicas, menciona la excepción después de `catch`:

```dart
try {
  var result = 100 ~/ 0;
} on IntegerDivisionByZeroException {
  print('No se puede dividir por cero.'); // Maneja específicamente las excepciones de división por cero
}
```
Salida de muestra: `No se puede dividir por cero.`

### Rastreo de Pila
Para obtener un rastreo de pila para la depuración, usa un segundo parámetro en el bloque catch:

```dart
try {
  var result = 100 ~/ 0;
} catch (e, s) {
  print('Excepción: $e');
  print('Rastreo de pila: $s'); // Imprime el rastreo de pila para depuración
}
```

### Finalmente
Usa `finally` para ejecutar código después de try/catch, independientemente de si se lanzó una excepción:

```dart
try {
  var result = 100 ~/ 0;
} catch (e) {
  print('Capturada una excepción: $e');
} finally {
  print('Esto siempre se ejecuta.'); // Código de limpieza o pasos finales
}
```
Salida de muestra:
```
Capturada una excepción: IntegerDivisionByZeroException
Esto siempre se ejecuta.
```

### Bibliotecas de Terceros
Aunque la biblioteca central de Dart es robusta para el manejo de errores, también puedes usar paquetes de terceros como `dartz` para la programación funcional, lo cual introduce conceptos como `Either` y `Option` que pueden ser usados para el manejo de errores. Aquí hay un ejemplo usando `dartz` para el manejo de errores:

1. Agrega `dartz` a tu archivo `pubspec.yaml` bajo dependencias:
```yaml
dependencies:
  dartz: ^0.10.0
```

2. Usa `Either` para manejar errores de manera elegante en tu código Dart:
```dart
import 'package:dartz/dartz.dart';

Either<String, int> divide(int dividendo, int divisor) {
  if (divisor == 0) {
    return Left('No se puede dividir por cero.');
  } else {
    return Right(dividendo ~/ divisor);
  }
}

void main() {
  final result = divide(100, 0);
  result.fold(
    (left) => print('Error: $left'), 
    (right) => print('Resultado: $right')
  );
}
```
Salida de muestra: `Error: No se puede dividir por cero.`

La parte `Left` usualmente representa el error, y la parte `Right` representa el éxito. Este patrón permite manejar errores de una manera más funcional, ofreciendo claridad y control sobre la gestión de errores.
