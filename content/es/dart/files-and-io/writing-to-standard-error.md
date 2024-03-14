---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:58:11.702543-07:00
description: "Escribir en el error est\xE1ndar (stderr) en Dart se trata de enviar\
  \ mensajes de error y diagn\xF3sticos a un flujo separado, distinto de la salida\
  \ est\xE1ndar\u2026"
lastmod: '2024-03-13T22:44:58.772053-06:00'
model: gpt-4-0125-preview
summary: "Escribir en el error est\xE1ndar (stderr) en Dart se trata de enviar mensajes\
  \ de error y diagn\xF3sticos a un flujo separado, distinto de la salida est\xE1\
  ndar\u2026"
title: "Escribiendo al error est\xE1ndar"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Escribir en el error estándar (stderr) en Dart se trata de enviar mensajes de error y diagnósticos a un flujo separado, distinto de la salida estándar (stdout). Los programadores hacen esto para diferenciar entre la salida normal del programa y los errores o mensajes de advertencia, permitiendo una depuración y un registro más fáciles.

## Cómo hacerlo:

En Dart, escribir en stderr es sencillo usando el objeto `stderr` disponible en `dart:io`. Aquí hay un ejemplo básico:

```dart
import 'dart:io';

void main() {
  stderr.writeln('Este es un mensaje de error.');
}
```

Salida al ejecutar:
```
Este es un mensaje de error.
```
Este mensaje se envía al flujo stderr, que típicamente se muestra en la consola o terminal.

Para demostrar más complejidad, como registrar una excepción, el rico conjunto de características de Dart permite un manejo de errores conciso y efectivo:

```dart
import 'dart:io';

void operacionRiesgosa() {
  try {
    // Simular una operación que podría lanzar una excepción
    throw Exception('¡Algo salió mal!');
  } catch (e) {
    stderr.writeln('Error: $e');
  }
}

void main() {
  operacionRiesgosa();
}
```

Salida al ejecutar:
```
Error: Exception: ¡Algo salió mal!
```

Este patrón es especialmente útil para aplicaciones que necesitan separar los registros normales de los registros de errores, facilitando el monitoreo y la depuración de aplicaciones.

Aunque la biblioteca estándar de Dart es bastante completa, muchos programas no requieren librerías de terceros para escribir en stderr. Sin embargo, si tu aplicación necesita capacidades de registro más sofisticadas (por ejemplo, a archivos, a través de la red, formateando), el paquete `logging` es una elección popular. Aquí tienes un vistazo rápido de cómo usar `logging` para errores:

```dart
import 'dart:io';
import 'package:logging/logging.dart';

final logger = Logger('MyAppLogger');

void setupLogging() {
  logger.onRecord.listen((registro) {
    if (registro.level >= Level.SEVERE) {
      stderr.writeln('${registro.level.name}: ${registro.time}: ${registro.message}');
    }
  });
}

void main() {
  setupLogging();
  logger.severe('Error Grave: Algo significativamente malo pasó.');
}
```

Salida al ejecutar:
```
SEVERE: 2023-04-01 00:00:00.000: Error Grave: Algo significativamente malo pasó.
```

Este método ofrece un mayor grado de personalización y control sobre qué se registra como un error y cómo se formatea, lo cual puede ser muy útil en aplicaciones más grandes y complejas.
