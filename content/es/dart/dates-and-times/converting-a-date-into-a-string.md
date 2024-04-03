---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:53:31.766794-07:00
description: "C\xF3mo hacerlo: Dart proporciona la clase `DateTime` para manejar fechas\
  \ y horas, y el paquete `intl` para formatear. Primero, aseg\xFArate de tener el\
  \ paquete\u2026"
lastmod: '2024-03-13T22:44:58.766330-06:00'
model: gpt-4-0125-preview
summary: Dart proporciona la clase `DateTime` para manejar fechas y horas, y el paquete
  `intl` para formatear.
title: Convirtiendo una fecha en una cadena de texto
weight: 28
---

## Cómo hacerlo:
Dart proporciona la clase `DateTime` para manejar fechas y horas, y el paquete `intl` para formatear. Primero, asegúrate de tener el paquete `intl` agregando `intl: ^0.17.0` (o la última versión) a tu archivo `pubspec.yaml`.

### Usando la Biblioteca Central de Dart
```dart
DateTime now = DateTime.now();
String formattedDate = "${now.year}-${now.month}-${now.day}";
print(formattedDate); // Salida: 2023-4-12 (por ejemplo, esto depende de la fecha actual)
```

Este ejemplo construye directamente una cadena a partir de las propiedades de `DateTime`.

### Usando el paquete `intl`
Primero, importa el paquete:

```dart
import 'package:intl/intl.dart';
```

Luego, formatea la fecha:

```dart
DateTime now = DateTime.now();
String formattedDate = DateFormat('yyyy-MM-dd').format(now);
print(formattedDate); // Salida: 2023-04-12
```

El paquete `intl` permite un formateo mucho más complejo fácilmente, incluyendo formatos específicos de la localidad:

```dart
String formattedDateLocale = DateFormat.yMMMMd('en_US').format(now);
print(formattedDateLocale); // Salida: April 12, 2023
```

Estos ejemplos muestran formas simples pero poderosas de convertir y formatear fechas en cadenas en Dart, ya sea utilizando la funcionalidad central de Dart o utilizando el paquete `intl` para opciones de formateo más avanzadas.
