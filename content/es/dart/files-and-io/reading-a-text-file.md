---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:27.601486-07:00
description: "Leer un archivo de texto en Dart implica acceder y recuperar datos de\
  \ archivos almacenados en el sistema de archivos. Los programadores hacen esto para\u2026"
lastmod: '2024-03-13T22:44:58.773116-06:00'
model: gpt-4-0125-preview
summary: Leer un archivo de texto en Dart implica acceder y recuperar datos de archivos
  almacenados en el sistema de archivos.
title: Leyendo un archivo de texto
weight: 22
---

## Cómo hacerlo:
La biblioteca central de Dart, `dart:io`, proporciona las funcionalidades necesarias para leer archivos de texto de manera sincrónica o asincrónica. A continuación, se explica cómo abordar ambas.

**De forma sincrónica:**

```dart
import 'dart:io';

void main() {
  var fileName = "ruta/a/tu/archivo_de_texto.txt";
  var file = File(fileName);

  // Leyendo el archivo de manera sincrónica
  var contents;
  try {
    contents = file.readAsStringSync();
    print(contents);
  } catch (e) {
    print('Error al leer el archivo: $e');
  }
}
```

**De forma asincrónica:**

Para evitar bloquear el programa mientras se lee el archivo, especialmente útil para archivos grandes o aplicaciones responsivas:

```dart
import 'dart:io';

void main() async {
  var fileName = "ruta/a/tu/archivo_de_texto.txt";
  var file = File(fileName);

  try {
    String contents = await file.readAsString();
    print(contents);
  } catch (e) {
    print('Error al leer el archivo: $e');
  }
}
```

**Ejemplo de Salida:**

Si tu archivo de texto contiene:

```
Hola, Dart!
```

Ambos métodos anteriores producirán:

```
Hola, Dart!
```

**Usando una Biblioteca de Terceros:**

Para características adicionales como operaciones de archivos simplificadas o manejo de errores mejorado, podrías considerar bibliotecas de terceros como `package:file`. Sin embargo, hasta mi última actualización, utilizar directamente el paquete central `dart:io`, como se muestra arriba, es el método más común y directo para leer archivos de texto en Dart.
