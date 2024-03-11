---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:53:39.753046-07:00
description: "Capitalizar una cadena implica modificar la primera letra de una palabra\
  \ o toda una oraci\xF3n a may\xFAsculas, mientras se mantienen el resto de los caracteres\u2026"
lastmod: '2024-03-11T00:14:32.554150-06:00'
model: gpt-4-0125-preview
summary: "Capitalizar una cadena implica modificar la primera letra de una palabra\
  \ o toda una oraci\xF3n a may\xFAsculas, mientras se mantienen el resto de los caracteres\u2026"
title: Capitalizando una cadena de texto
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Capitalizar una cadena implica modificar la primera letra de una palabra o toda una oración a mayúsculas, mientras se mantienen el resto de los caracteres tal como están. Los programadores a menudo usan esta técnica para formatear entradas de usuarios o mostrar texto para asegurar consistencia o adherirse a reglas gramaticales en interfaces de usuario.

## Cómo:

### Usando los Métodos Incorporados de Dart

Dart ofrece métodos simples y directos para la manipulación de cadenas. Para capitalizar una palabra o una oración, típicamente se toma el primer carácter, se convierte a mayúsculas, y luego se concatena con el resto de la cadena. Así es como podrías implementarlo:

```dart
String capitalize(String text) {
  if (text.isEmpty) return text;
  return text[0].toUpperCase() + text.substring(1).toLowerCase();
}

void main() {
  var example = "hello world";
  print(capitalize(example)); // Salida: Hello world
}
```

### Capitalizando Cada Palabra

Para capitalizar la primera letra de cada palabra en una cadena, podrías dividir la cadena en palabras, capitalizar cada una, y luego unirlas de nuevo:

```dart
String capitalizeWords(String text) {
  return text.split(' ').map(capitalize).join(' ');
}

void main() {
  var example = "hello dart enthusiasts";
  print(capitalizeWords(example)); // Salida: Hello Dart Enthusiasts
}
```

### Usando Bibliotecas de Terceros

Aunque la biblioteca estándar de Dart cubre las necesidades básicas, ciertas tareas podrían realizarse más convenientemente utilizando paquetes de terceros. Una elección popular para capacidades extendidas de manipulación de cadenas, incluyendo la capitalización, es el paquete [`recase`](https://pub.dev/packages/recase). Después de añadirlo al `pubspec.yaml` de tu proyecto, puedes capitalizar cadenas fácilmente entre otras funcionalidades:

```dart
import 'package:recase/recase.dart';

void main() {
  var example = "hello world";
  var rc = ReCase(example);

  print(rc.titleCase); // Salida: Hello World
}
```

Usando `recase`, puedes capitalizar palabras individuales, oraciones enteras, o incluso seguir otras convenciones de casos sin manejar manualmente las transformaciones de las cadenas.
