---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:57:44.734963-07:00
description: "Trabalhar com JSON (JavaScript Object Notation) envolve a convers\xE3\
  o de dados JSON de strings para objetos Dart e vice-versa, uma tarefa comum no\u2026"
lastmod: '2024-03-13T22:44:46.304767-06:00'
model: gpt-4-0125-preview
summary: "Trabalhar com JSON (JavaScript Object Notation) envolve a convers\xE3o de\
  \ dados JSON de strings para objetos Dart e vice-versa, uma tarefa comum no\u2026"
title: Trabalhando com JSON
weight: 38
---

## O Que & Por Quê?

Trabalhar com JSON (JavaScript Object Notation) envolve a conversão de dados JSON de strings para objetos Dart e vice-versa, uma tarefa comum no desenvolvimento de web e aplicativos para a troca de dados. Os programadores fazem isso para manipular dados de APIs, configurações ou comunicação entre componentes dentro de seus aplicativos de forma eficiente.

## Como fazer:

Dart oferece suporte integrado para JSON com a biblioteca `dart:convert`, facilitando a codificação e decodificação de JSON. Abaixo estão exemplos que demonstram operações básicas:

**Analisando String JSON para Objeto Dart:**
```dart
import 'dart:convert';

void main() {
  // Exemplo de string JSON
  String jsonString = '{"name": "John", "age": 30, "email": "john@example.com"}';
  
  // Decodificando JSON para Map Dart
  Map<String, dynamic> user = jsonDecode(jsonString);
  
  print('Olá, ${user['name']}! Você tem ${user['age']} anos.');
  // Saída: Olá, John! Você tem 30 anos.
}
```

**Codificando Objeto Dart para String JSON:**
```dart
import 'dart:convert';

void main() {
  // Exemplo de objeto Dart
  Map<String, dynamic> user = {
    'name': 'Jane',
    'age': 25,
    'email': 'jane@example.com'
  };
  
  // Codificando Map Dart para JSON
  String jsonString = jsonEncode(user);
  
  print(jsonString);
  // Saída: {"name":"Jane","age":25,"email":"jane@example.com"}
}
```

**Usando `json_serializable` para Modelos Complexos:**
Para modelos de dados complexos, a serialização manual pode ser trabalhosa. O pacote `json_serializable` automatiza esse processo. Requer configuração adicional, incluindo adicionar dependências ao seu `pubspec.yaml` e criar arquivos de construção. Após a configuração, você pode usá-lo da seguinte forma:

1. Defina um modelo com anotações:
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

2. Gere o código de serialização:
Use o comando do build runner para gerar o arquivo `user.g.dart`:
```shell
flutter pub run build_runner build
```

3. Use seu modelo:
```dart
void main() {
  // Analisando JSON para User
  Map userMap = jsonDecode('{"name": "John", "age": 30, "email": "john@example.com"}');
  User user = User.fromJson(userMap);
  
  print('Usuário: ${user.name}, Idade: ${user.age}');
  // Saída: Usuário: John, Idade: 30

  // Convertendo User de volta para JSON
  String jsonString = jsonEncode(user.toJson());
  print(jsonString);
  // Saída: {"name":"John","age":30,"email":"john@example.com"}
}
```

Esses exemplos ilustram interações básicas e avançadas com JSON em Dart, capacitando desenvolvedores a lidar com tarefas de serialização de dados em seus aplicativos de maneira contínua.
