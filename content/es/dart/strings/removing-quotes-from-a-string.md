---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:51.807101-07:00
description: "C\xF3mo hacerlo: Dart proporciona maneras sencillas de eliminar comillas\
  \ de una cadena usando m\xE9todos de cadena incorporados sin necesidad de bibliotecas\
  \ de\u2026"
lastmod: '2024-03-13T22:44:58.735325-06:00'
model: gpt-4-0125-preview
summary: "Dart proporciona maneras sencillas de eliminar comillas de una cadena usando\
  \ m\xE9todos de cadena incorporados sin necesidad de bibliotecas de terceros."
title: Eliminando comillas de una cadena de texto
weight: 9
---

## Cómo hacerlo:
Dart proporciona maneras sencillas de eliminar comillas de una cadena usando métodos de cadena incorporados sin necesidad de bibliotecas de terceros.

### Ejemplo 1: Usando `replaceFirst` y `replaceAll`
Si estás tratando con cadenas que comienzan y terminan con comillas, puedes usar los métodos `replaceFirst` y `replaceAll` para eliminarlas.

```dart
String quotedString = '"Hola, Mundo!"';
String singleQuotedString = '\'Programación Dart\'';

// Eliminando comillas dobles
String noDoubleQuotes = quotedString.replaceFirst('"', '').replaceAll('"', '');
print(noDoubleQuotes); // Salida: Hola, Mundo!

// Eliminando comillas simples
String noSingleQuotes = singleQuotedString.replaceFirst('\'', '').replaceAll('\'', '');
print(noSingleQuotes); // Salida: Programación Dart
```

### Ejemplo 2: Usando `substring`
Este método es útil cuando estás seguro de que las comillas están justo al inicio y al final de la cadena.

```dart
String quotedString = '"Desarrollo Flutter"';
// Verifica si comienza y termina con comillas antes de eliminar para evitar errores
if (quotedString.startsWith('"') && quotedString.endsWith('"')) {
  quotedString = quotedString.substring(1, quotedString.length - 1);
}
print(quotedString); // Salida: Desarrollo Flutter
```

### Ejemplo 3: Método de Extensión Personalizado
Para mayor reutilización, particularmente si tu proyecto implica la eliminación frecuente de comillas, considera crear una extensión personalizada en `String`.

```dart
extension UnquoteString on String {
  String unquote() {
    var str = this;
    if (str.startsWith('"') && str.endsWith('"') || str.startsWith('\'') && str.endsWith('\'')) {
      str = str.substring(1, str.length - 1);
    }
    return str;
  }
}

void main() {
  String doubleQuoted = '"Esto es Dart"';
  String singleQuoted = '\'Esto es asombroso\'';
  print(doubleQuoted.unquote()); // Salida: Esto es Dart
  print(singleQuoted.unquote()); // Salida: Esto es asombroso
}
```

Estos enfoques deberían ayudarte a eliminar comillas de las cadenas de manera efectiva en Dart, mejorando tus flujos de trabajo de procesamiento y preparación de datos.
