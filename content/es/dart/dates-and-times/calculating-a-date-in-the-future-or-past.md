---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:53:33.023731-07:00
description: "Calcular una fecha en el futuro o en el pasado es una tarea com\xFA\
  n para los programadores, que se enfrentan a la programaci\xF3n de agendas, recordatorios\
  \ o\u2026"
lastmod: '2024-03-13T22:44:58.768727-06:00'
model: gpt-4-0125-preview
summary: "Calcular una fecha en el futuro o en el pasado es una tarea com\xFAn para\
  \ los programadores, que se enfrentan a la programaci\xF3n de agendas, recordatorios\
  \ o cualquier caracter\xEDstica que dependa del c\xE1lculo de fechas."
title: "C\xE1lculo de una fecha en el futuro o pasado"
weight: 26
---

## ¿Qué y Por Qué?
Calcular una fecha en el futuro o en el pasado es una tarea común para los programadores, que se enfrentan a la programación de agendas, recordatorios o cualquier característica que dependa del cálculo de fechas. Entender cómo manipular las fechas es crucial para los sistemas de backend, las interfaces de usuario y el análisis de datos, especialmente para aquellos que están haciendo la transición a Dart y buscan implementar la lógica temporal de manera eficiente.

## Cómo hacerlo:
Dart proporciona un soporte robusto para la manipulación de fechas a través de su clase `DateTime`. Aquí te mostramos cómo puedes calcular fechas futuras o pasadas usando Dart nativo, sin necesidad de librerías de terceros.

### Calcular una Fecha Futura
Para calcular una fecha en el futuro, creas un objeto `DateTime` y usas el método `add` con la duración deseada.

```dart
DateTime hoy = DateTime.now();
Duration diezDias = Duration(days: 10);
DateTime fechaFutura = hoy.add(diezDias);

print(fechaFutura); // Salida: 2023-04-21 14:22:35.123456 (salida de ejemplo, depende de la fecha y hora actuales)
```

### Calcular una Fecha Pasada
Para calcular una fecha en el pasado, usas el método `subtract` en un objeto `DateTime` con la duración necesaria.

```dart
DateTime hoy = DateTime.now();
Duration quinceDiasAtras = Duration(days: 15);
DateTime fechaPasada = hoy.subtract(quinceDiasAtras);

print(fechaPasada); // Salida: 2023-03-27 14:22:35.123456 (salida de ejemplo, depende de la fecha y hora actuales)
```

### Usar Librerías de Terceros
Aunque las capacidades nativas de Dart para la manipulación de fechas son poderosas, podrías encontrarte necesitando operaciones más específicas, como parsear o formatear fechas más fácilmente, o realizar cálculos complejos. En tales casos, el paquete `time` puede ser muy útil.

Primero, añade `time` a tus dependencias en `pubspec.yaml`:

```yaml
dependencies:
  time: ^2.0.0
```

Luego, puedes usarlo para realizar cálculos similares con una legibilidad mejorada:

```dart
import 'package:time/time.dart';

void main() {
  DateTime hoy = DateTime.now();

  // Calcular una fecha futura
  DateTime fechaFutura = hoy + 10.days;
  print(fechaFutura); // Formato de salida: 2023-04-21 14:22:35.123456

  // Calcular una fecha pasada
  DateTime fechaPasada = hoy - 15.days;
  print(fechaPasada); // Formato de salida: 2023-03-27 14:22:35.123456
}
```

Estos ejemplos ilustran manipulaciones básicas de fechas en Dart, incluyendo la adición y sustracción de tiempo a o desde una fecha actual, demostrando lo fácil que pueden ser gestionadas las fechas en aplicaciones Dart.
