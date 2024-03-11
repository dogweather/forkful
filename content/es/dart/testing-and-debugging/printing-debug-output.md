---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:22.101037-07:00
description: "Imprimir salida de depuraci\xF3n en Dart es acerca de mostrar informaci\xF3\
  n en la consola durante el tiempo de ejecuci\xF3n, permitiendo a los desarrolladores\u2026"
lastmod: '2024-03-11T00:14:32.575900-06:00'
model: gpt-4-0125-preview
summary: "Imprimir salida de depuraci\xF3n en Dart es acerca de mostrar informaci\xF3\
  n en la consola durante el tiempo de ejecuci\xF3n, permitiendo a los desarrolladores\u2026"
title: "Imprimiendo salida de depuraci\xF3n"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

Imprimir salida de depuración en Dart es acerca de mostrar información en la consola durante el tiempo de ejecución, permitiendo a los desarrolladores rastrear el flujo de ejecución, investigar el estado de las variables, o identificar la fuente de errores. Los programadores comúnmente lo usan para la resolución de problemas y verificar que su código se comporte como se espera, facilitando un proceso de desarrollo más suave y eficiente.

## Cómo hacerlo:

En Dart, puedes imprimir la salida de depuración usando la función `print()`. Aquí te mostramos cómo sacar mensajes simples y valores de variables:

```dart
void main() {
  String saludo = "¡Hola, Dart!";
  print(saludo); // Imprime: ¡Hola, Dart!

  int numero = 42;
  print('El número es $numero.'); // Imprime: El número es 42.
}
```

Para datos estructurados, como listas u objetos, el método `toString()` de Dart puede no proporcionar suficiente detalle. En esos casos, puedes utilizar la función `jsonEncode` de la biblioteca `dart:convert` de Dart para convertir los datos a una cadena JSON para una salida más legible:

```dart
import 'dart:convert';

void main() {
  var usuario = {
    'nombre': 'John Doe',
    'edad': 30,
    'correos': ['john.doe@example.com', 'john@example.com'],
  };

  print(jsonEncode(usuario));
  // Imprime: {"nombre":"John Doe","edad":30,"correos":["john.doe@example.com","john@example.com"]}
}
```

Cuando se necesiten capacidades de depuración más sofisticadas, como el registro con diferentes niveles de importancia (información, advertencia, error), puedes usar bibliotecas de terceros como `logger`. Aquí te mostramos cómo usarla:

1. Añade `logger` a tu `pubspec.yaml`:

```yaml
dependencies:
  logger: ^1.0.0
```

2. Usa `logger` en tu código Dart:

```dart
import 'package:logger/logger.dart';

var logger = Logger();

void main() {
  logger.d("Este es un mensaje de depuración");
  logger.w("Este es un mensaje de advertencia");
  logger.e("Este es un mensaje de error");
}
```

La salida será más informativa, mostrando el nivel del mensaje y el mensaje en sí, facilitando la distinción entre diferentes tipos de mensajes de registro.
