---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:15.532259-07:00
description: "C\xF3mo hacerlo: En Dart, puedes usar el paquete `http` para enviar\
  \ solicitudes HTTP con autenticaci\xF3n b\xE1sica. Primero, agrega el paquete `http`\
  \ a tu archivo\u2026"
lastmod: '2024-03-13T22:44:58.750620-06:00'
model: gpt-4-0125-preview
summary: "En Dart, puedes usar el paquete `http` para enviar solicitudes HTTP con\
  \ autenticaci\xF3n b\xE1sica."
title: "Enviando una solicitud HTTP con autenticaci\xF3n b\xE1sica"
weight: 45
---

## Cómo hacerlo:
En Dart, puedes usar el paquete `http` para enviar solicitudes HTTP con autenticación básica. Primero, agrega el paquete `http` a tu archivo `pubspec.yaml`:

```yaml
dependencies:
  http: ^0.13.4
```

Luego, importa el paquete en tu archivo Dart:

```dart
import 'package:http/http.dart' as http;
import 'dart:convert';
```

Para enviar una solicitud GET con autenticación básica, puedes usar el siguiente código:

```dart
Future<void> fetchUserData() async {
  final username = 'tuNombreDeUsuario';
  final password = 'tuContraseña';
  final credentials = base64Encode(utf8.encode('$username:$password'));
  final response = await http.get(
    Uri.parse('https://tuapi.com/datosdeusuario'),
    headers: {
      'Authorization': 'Basic $credentials',
    },
  );

  if (response.statusCode == 200) {
    print('¡Datos del usuario obtenidos con éxito!');
    print('Cuerpo de la respuesta: ${response.body}');
  } else {
    print('Error al obtener datos del usuario con código de estado: ${response.statusCode}');
  }
}
```

Este código envía una solicitud GET a 'https://tuapi.com/datosdeusuario' con un encabezado de autenticación básica. El nombre de usuario y la contraseña se codifican en base64 y se pasan en el encabezado 'Authorization' según los estándares de autenticación de acceso básico.

**Salida de muestra:**

Tras una solicitud exitosa y si el servidor devuelve un código de estado de 200, podrías ver:

```plaintext
¡Datos del usuario obtenidos con éxito!
Cuerpo de la respuesta: {"id":1, "nombre":"John Doe", "correo":"john@example.com"}
```

Si la autenticación falla o hay algún otro error, el código de estado de la respuesta ayudará a identificar el problema.
