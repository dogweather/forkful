---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:54:54.399823-07:00
description: "C\xF3mo hacerlo: La biblioteca central de Dart proporciona acceso directo\
  \ a la fecha y hora actuales a trav\xE9s de la clase `DateTime`. Aqu\xED tienes\
  \ el ejemplo\u2026"
lastmod: '2024-03-13T22:44:58.764848-06:00'
model: gpt-4-0125-preview
summary: "La biblioteca central de Dart proporciona acceso directo a la fecha y hora\
  \ actuales a trav\xE9s de la clase `DateTime`."
title: Obteniendo la fecha actual
weight: 29
---

## Cómo hacerlo:
La biblioteca central de Dart proporciona acceso directo a la fecha y hora actuales a través de la clase `DateTime`. Aquí tienes el ejemplo básico para obtener la fecha actual:

```dart
void main() {
  DateTime now = DateTime.now();
  print(now); // Ejemplo de salida: 2023-04-12 10:00:00.000
}
```

Si solo necesitas la parte de la fecha (año, mes, día), puedes formatear el objeto `DateTime`:

```dart
void main() {
  DateTime now = DateTime.now();
  String formattedDate = "${now.year}-${now.month}-${now.day}";
  print(formattedDate); // Ejemplo de salida: 2023-04-12
}
```

Dart no incluye una biblioteca integrada para un formateo de fecha más complejo, pero puedes usar el paquete `intl` para este propósito. Primero, agrega el paquete a tu `pubspec.yaml`:

```yaml
dependencies:
  intl: ^0.17.0
```

Luego, puedes formatear fechas fácilmente:

```dart
import 'package:intl/intl.dart';

void main() {
  DateTime now = DateTime.now();
  String formattedDate = DateFormat('yyyy-MM-dd').format(now);
  print(formattedDate); // Ejemplo de salida: 2023-04-12
}
```

Para opciones de formateo más avanzadas, explora la clase `DateFormat` proporcionada por el paquete `intl`, la cual soporta una amplia gama de patrones y locales.
