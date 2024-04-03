---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:12.723996-07:00
description: "Enviar una solicitud HTTP en Dart es el proceso de iniciar comunicaciones\
  \ con un servidor web o API desde una aplicaci\xF3n Dart. Los programadores lo hacen\u2026"
lastmod: '2024-03-13T22:44:58.746774-06:00'
model: gpt-4-0125-preview
summary: "Enviar una solicitud HTTP en Dart es el proceso de iniciar comunicaciones\
  \ con un servidor web o API desde una aplicaci\xF3n Dart."
title: Enviando una solicitud HTTP
weight: 44
---

## ¿Qué y por qué?

Enviar una solicitud HTTP en Dart es el proceso de iniciar comunicaciones con un servidor web o API desde una aplicación Dart. Los programadores lo hacen para obtener datos de la web, enviar formularios e interactuar con servicios RESTful, lo que lo convierte en una operación fundamental para el desarrollo de aplicaciones web, del lado del servidor y móviles en Dart.

## Cómo hacerlo:

Dart incluye el paquete `http`, una forma poderosa y conveniente de trabajar con recursos HTTP. Primero, inclúyelo en tu archivo pubspec.yaml:

```yaml
dependencies:
  http: ^0.13.3
```

Luego, impórtalo en tu código Dart para comenzar a hacer solicitudes:

```dart
import 'package:http/http.dart' as http;

void main() async {
  var url = Uri.parse('https://jsonplaceholder.typicode.com/todos/1');
  var response = await http.get(url);

  if (response.statusCode == 200) {
    print('Cuerpo de la respuesta: ${response.body}');
  } else {
    print('La solicitud falló con el estado: ${response.statusCode}.');
  }
}
```

Un ejemplo de salida para una solicitud exitosa podría verse así:

```
Cuerpo de la respuesta: {
  "userId": 1,
  "id": 1,
  "title": "delectus aut autem",
  "completed": false
}
```

Para solicitudes más complejas, como solicitudes POST con un cuerpo JSON, harías lo siguiente:

```dart
import 'dart:convert';
import 'package:http/http.dart' as http;

void main() async {
  var url = Uri.parse('https://jsonplaceholder.typicode.com/posts');
  var response = await http.post(
    url,
    headers: {"Content-Type": "application/json"},
    body: jsonEncode({
      "title": 'foo',
      "body": 'bar',
      "userId": 1,
    }),
  );

  if (response.statusCode == 201) {
    print('Estado de la respuesta: ${response.statusCode}');
    print('Cuerpo de la respuesta: ${response.body}');
  } else {
    print('Falló al crear una nueva publicación. Estado: ${response.statusCode}');
  }
}
```

Un ejemplo de salida para la solicitud post podría ser:

```
Estado de la respuesta: 201
Cuerpo de la respuesta: {
  "title": "foo",
  "body": "bar",
  "userId": 1,
  "id": 101
}
```

Estos ejemplos muestran solicitudes HTTP GET y POST básicas usando el paquete `http` en Dart. Este paquete cubre la mayoría de las necesidades para enviar solicitudes HTTP, incluidos escenarios más complejos con encabezados y contenido del cuerpo.
