---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:35.146532-07:00
description: "C\xF3mo: Dart no viene con un REPL incorporado. Sin embargo, puedes\
  \ lograr una funcionalidad similar a REPL utilizando DartPad (en l\xEDnea) o mediante\u2026"
lastmod: '2024-03-13T22:44:58.753272-06:00'
model: gpt-4-0125-preview
summary: Dart no viene con un REPL incorporado.
title: Usando un shell interactivo (REPL)
weight: 34
---

## Cómo:
Dart no viene con un REPL incorporado. Sin embargo, puedes lograr una funcionalidad similar a REPL utilizando DartPad (en línea) o mediante herramientas de terceros como `dart_repl`.

**Usando DartPad:**

DartPad (https://dartpad.dev) es un editor de Dart en línea que te permite escribir y ejecutar código Dart en tu navegador web. Aunque no es un REPL tradicional de línea de comando, proporciona una experiencia similar para la experimentación rápida.

Simplemente ve al sitio web, escribe tu código Dart en el panel izquierdo y haz clic en "Ejecutar" para ver la salida en el derecho.

Ejemplo:
```dart
void main() {
  print('Hola, Dart!');
}
```
Salida:
```
Hola, Dart!
```

**Usando `dart_repl` (herramienta de terceros):**

Primero, instala `dart_repl` a través de pub de manera global:

```shell
dart pub global activate dart_repl
```

Luego, corre `dart_repl` desde tu terminal:

```shell
dart_repl
```

Ahora, puedes comenzar a escribir declaraciones de Dart directamente en el shell. Por ejemplo:

```dart
>>> print('¡Hola, REPL!');
Hola, REPL!
>>> int add(int x, int y) => x + y;
>>> print(add(5, 7));
12
```

Estos métodos proporcionan un camino rápido para probar código Dart al momento, facilitando significativamente la curva de aprendizaje y mejorando la productividad.
