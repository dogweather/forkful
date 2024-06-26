---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:57:08.717863-07:00
description: "C\xF3mo hacerlo: Dart provee soporte incorporado para JSON con la biblioteca\
  \ `dart:convert`, lo que hace que codificar y decodificar JSON sea directo. A\u2026"
lastmod: '2024-03-13T22:44:58.777604-06:00'
model: gpt-4-0125-preview
summary: Dart provee soporte incorporado para JSON con la biblioteca `dart:convert`,
  lo que hace que codificar y decodificar JSON sea directo.
title: Trabajando con JSON
weight: 38
---

## Cómo hacerlo:
Dart provee soporte incorporado para JSON con la biblioteca `dart:convert`, lo que hace que codificar y decodificar JSON sea directo. A continuación, se muestran ejemplos que destacan operaciones básicas:

**Analizando Cadena JSON a Objeto Dart:**
```dart
import 'dart:convert';

void main() {
  // Ejemplo de cadena JSON
  String jsonString = '{"name": "John", "age": 30, "email": "john@example.com"}';
  
  // Decodificando JSON a Mapa Dart
  Map<String, dynamic> user = jsonDecode(jsonString);
  
  print('Hola, ${user['name']}! Tienes ${user['age']} años.');
  // Salida: Hola, John! Tienes 30 años.
}
```

**Codificando Objeto Dart a Cadena JSON:**
```dart
import 'dart:convert';

void main() {
  // Ejemplo de objeto Dart
  Map<String, dynamic> user = {
    'name': 'Jane',
    'age': 25,
    'email': 'jane@example.com'
  };
  
  // Codificando Mapa Dart a JSON
  String jsonString = jsonEncode(user);
  
  print(jsonString);
  // Salida: {"name":"Jane","age":25,"email":"jane@example.com"}
}
```

**Usando `json_serializable` para Modelos Complejos:**
Para modelos de datos complejos, la serialización manual puede ser engorrosa. El paquete `json_serializable` automatiza este proceso. Requiere una configuración adicional, incluyendo agregar dependencias a tu `pubspec.yaml` y crear archivos de construcción. Después de la configuración, puedes usarlo de la siguiente manera:

1. Definir un modelo con anotaciones:
```dart
import 'package:json_annotation/json_annotation.dart';

part 'user.g.dart';

@JsonSerializable()
class User {
  String name;
  int age;
  String email;
  
  User({required this.name, required this.age, required this.email});
  
  factory User.fromJson(Map<String, dynamic> json) => _$UserFromJson(json);
  Map<String, dynamic> toJson() => _$UserToJson(this);
}
```

2. Generar la infraestructura de serialización:
Usa el comando del creador de compilaciones para generar el archivo `user.g.dart`:
```shell
flutter pub run build_runner build
```

3. Usar tu modelo:
```dart
void main() {
  // Analizando JSON a Usuario
  Map userMap = jsonDecode('{"name": "John", "age": 30, "email": "john@example.com"}');
  User user = User.fromJson(userMap);
  
  print('Usuario: ${user.name}, Edad: ${user.age}');
  // Salida: Usuario: John, Edad: 30

  // Convirtiendo Usuario de vuelta a JSON
  String jsonString = jsonEncode(user.toJson());
  print(jsonString);
  // Salida: {"name":"John","age":30,"email":"john@example.com"}
}
```

Estos ejemplos ilustran interacciones básicas y avanzadas con JSON en Dart, empoderando a los desarrolladores para manejar tareas de serialización de datos en sus aplicaciones sin problemas.
