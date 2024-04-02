---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:54:58.472223-07:00
description: "La interpolaci\xF3n de cadenas es el proceso de inyectar valores de\
  \ variables directamente en cadenas de texto, a menudo para crear mensajes significativos\u2026"
lastmod: '2024-03-13T22:44:58.732967-06:00'
model: gpt-4-0125-preview
summary: "La interpolaci\xF3n de cadenas es el proceso de inyectar valores de variables\
  \ directamente en cadenas de texto, a menudo para crear mensajes significativos\u2026"
title: Interpolando una cadena de texto
weight: 8
---

## ¿Qué y por qué?

La interpolación de cadenas es el proceso de inyectar valores de variables directamente en cadenas de texto, a menudo para crear mensajes significativos sin concatenaciones engorrosas. Los programadores lo hacen para tener un código más limpio, legible y para prevenir errores propensos a ocurrir en concatenaciones de cadenas complejas.

## Cómo hacerlo:

En Dart, la interpolación de cadenas es sencilla, utilizando el símbolo `$` para interpolar expresiones directamente dentro de literales de cadena:

```dart
void main() {
  String name = 'Dart';
  int year = 2023;
  // Interpolación simple de variables
  print('Aprendiendo $name en $year!');
  // Salida: Aprendiendo Dart en 2023!
  
  // Interpolando expresiones
  print('En dos años, será ${year + 2}.');
  // Salida: En dos años, será 2025.
}
```

En el caso de que tengas expresiones más complejas o quieras realizar operaciones dentro de la propia cadena, encierra la expresión en `${}`. Dart no tiene bibliotecas de terceros populares específicamente para la interpolación de cadenas ya que está bien equipado de forma nativa para manejar escenarios variados y complejos.
