---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:53.040707-07:00
description: "C\xF3mo hacerlo: Dart proporciona un enfoque sencillo para acceder a\
  \ los argumentos de la l\xEDnea de comandos a trav\xE9s de la `List<String> args`\
  \ en el m\xE9todo\u2026"
lastmod: '2024-03-13T22:44:58.770935-06:00'
model: gpt-4-0125-preview
summary: "Dart proporciona un enfoque sencillo para acceder a los argumentos de la\
  \ l\xEDnea de comandos a trav\xE9s de la `List<String> args` en el m\xE9todo principal."
title: "Leyendo argumentos de la l\xEDnea de comandos"
weight: 23
---

## Cómo hacerlo:
Dart proporciona un enfoque sencillo para acceder a los argumentos de la línea de comandos a través de la `List<String> args` en el método principal. A continuación, se muestra un ejemplo simple que demuestra cómo leer y utilizar los argumentos de la línea de comandos.

```dart
// main.dart
void main(List<String> args) {
  print('Argumentos de la Línea de Comandos:');
  for (var i = 0; i < args.length; i++) {
    print('${i + 1}: ${args[i]}');
  }
}
```

Para ejecutar este programa Dart y pasar argumentos de línea de comandos, usa la CLI de Dart de la siguiente manera:

```shell
dart run main.dart Hello World!
```

Salida esperada:

```
Argumentos de la Línea de Comandos:
1: Hello
2: World!
```

### Usando una Biblioteca de Terceros Popular: `args`
Aunque las capacidades integradas de Dart para manejar argumentos de línea de comandos son robustas para muchas aplicaciones, el paquete `args` proporciona una forma refinada de definir y analizar argumentos de línea de comandos para necesidades más complejas.

Primero, añade el paquete `args` a tu `pubspec.yaml`:

```yaml
dependencies:
  args: ^2.0.0
```

Luego, úsalo en tu programa de la siguiente manera:

```dart
// Usando el paquete 'args'
import 'package:args/args.dart';

void main(List<String> arguments) {
  final parser = ArgParser()..addOption('name', abbr: 'n');
  final argResults = parser.parse(arguments);

  if (argResults.wasParsed('name')) {
    print('Hola, ${argResults['name']}!');
  } else {
    print('No se proporcionó nombre.');
  }
}
```

Ejecuta el programa con un argumento nombrado:

```shell
dart run main.dart --name=John
```

Salida esperada:

```
Hola, John!
```

Esta introducción simple al análisis de argumentos de línea de comandos, tanto de forma nativa como con la biblioteca `args`, muestra cómo Dart puede manejar las entradas de usuario directamente desde la consola, abriendo un camino hacia la creación de aplicaciones CLI más interactivas y dinámicas.
