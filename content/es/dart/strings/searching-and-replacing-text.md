---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:03.751537-07:00
description: "Buscar y reemplazar texto en Dart implica examinar cadenas para encontrar\
  \ ciertos patrones o secuencias de caracteres y sustituirlos con nuevo contenido.\u2026"
lastmod: '2024-03-13T22:44:58.731633-06:00'
model: gpt-4-0125-preview
summary: Buscar y reemplazar texto en Dart implica examinar cadenas para encontrar
  ciertos patrones o secuencias de caracteres y sustituirlos con nuevo contenido.
title: Buscando y reemplazando texto
weight: 10
---

## Cómo hacerlo:
Dart ofrece métodos robustos para buscar y reemplazar texto directamente a través de su clase `String`, sin necesidad de bibliotecas externas. Así es como puedes hacerlo:

### Búsqueda y Reemplazo Básicos
Para buscar una subcadena y reemplazarla con otra cadena, puedes usar `replaceAll`:

```dart
String textoEjemplo = "Hola, Dart! Dart es genial.";
String textoModificado = textoEjemplo.replaceAll("Dart", "Flutter");
print(textoModificado); // Salida: Hola, Flutter! Flutter es genial.
```

### Uso de Expresiones Regulares
Para necesidades de búsqueda y reemplazo más complejas, Dart utiliza expresiones regulares a través de la clase `RegExp`. Esto permite el emparejamiento de patrones y reemplazo en cadenas:

```dart
String textoEjemplo = "Dart 2023, Flutter 2023";
String textoModificado = textoEjemplo.replaceAll(RegExp(r'\d+'), "2024");
print(textoModificado); // Salida: Dart 2024, Flutter 2024
```

Este ejemplo encuentra todas las instancias de uno o más dígitos (`\d+`) en la cadena y las reemplaza con "2024".

### Búsqueda Insensible a Mayúsculas y Minúsculas
Para realizar una búsqueda que no distinga entre mayúsculas y minúsculas, puedes modificar el constructor `RegExp` para ignorar el caso:

```dart
String textoEjemplo = "Bienvenido a Dart, el lenguaje de programación.";
String textoModificado = textoEjemplo.replaceAll(RegExp(r'dart', caseSensitive: false), "Flutter");
print(textoModificado); // Salida: Bienvenido a Flutter, el lenguaje de programación.
```

### Reemplazando con una Función
Para reemplazos dinámicos basados en la coincidencia misma, Dart permite pasar una función a `replaceAllMapped`. Esta función puede realizar operaciones o cálculos sobre las secuencias coincidentes:

```dart
String textoEjemplo = "Incrementa 5 por 1 para obtener 6.";
String textoIncrementado = textoEjemplo.replaceAllMapped(RegExp(r'\d+'), (Match m) => (int.parse(m[0]!) + 1).toString());
print(textoIncrementado); // Salida: Incrementa 6 por 1 para obtener 7.
```

Esto reemplaza cada secuencia de dígitos con su valor incrementado. Cada coincidencia se analiza en un entero, se incrementa, y luego se convierte de nuevo a una cadena para el reemplazo.

Las capacidades de manipulación de cadenas de Dart, especialmente para buscar y reemplazar texto, lo convierten en una herramienta potente para procesar y preparar datos dentro de tus aplicaciones. Ya sea utilizando reemplazos de cadenas directas o aprovechando el poder de las expresiones regulares, Dart proporciona la flexibilidad y rendimiento necesarios para una manipulación de texto efectiva.
