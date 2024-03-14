---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:35.146532-07:00
description: "Un shell interactivo (REPL - Bucle Leer-Evaluar-Imprimir) para Dart\
  \ permite a los programadores escribir y ejecutar c\xF3digo Dart l\xEDnea por l\xED\
  nea sin\u2026"
lastmod: '2024-03-13T22:44:58.753272-06:00'
model: gpt-4-0125-preview
summary: "Un shell interactivo (REPL - Bucle Leer-Evaluar-Imprimir) para Dart permite\
  \ a los programadores escribir y ejecutar c\xF3digo Dart l\xEDnea por l\xEDnea sin\u2026"
title: Usando un shell interactivo (REPL)
---

{{< edit_this_page >}}

## Qué y Por Qué?

Un shell interactivo (REPL - Bucle Leer-Evaluar-Imprimir) para Dart permite a los programadores escribir y ejecutar código Dart línea por línea sin necesidad de compilar scripts enteros. Esta herramienta es invaluable para aprender la sintaxis de Dart, experimentar con fragmentos de código o depurar ofreciendo retroalimentación instantánea y facilitando pruebas iterativas.

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
