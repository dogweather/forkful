---
title:                "Working with JSON"
date:                  2024-03-08T21:33:54.589252-07:00
model:                 gpt-4-0125-preview
---

{{< edit_this_page >}}

## What & Why?

Working with JSON (JavaScript Object Notation) involves parsing JSON data from strings into Dart objects and vice versa, a common task in web and app development for data interchange. Programmers do it to efficiently handle data from APIs, configurations, or inter-component communication within their apps.

## How to:

Dart provides built-in support for JSON with the `dart:convert` library, making it straightforward to encode and decode JSON. Below are examples showcasing basic operations:

**Parsing JSON String to Dart Object:**
```dart
import 'dart:convert';

void main() {
  // Example JSON string
  String jsonString = '{"name": "John", "age": 30, "email": "john@example.com"}';
  
  // Decoding JSON to Dart Map
  Map<String, dynamic> user = jsonDecode(jsonString);
  
  print('Hello, ${user['name']}! You are ${user['age']} years old.');
  // Output: Hello, John! You are 30 years old.
}
```

**Encoding Dart Object to JSON String:**
```dart
import 'dart:convert';

void main() {
  // Example Dart object
  Map<String, dynamic> user = {
    'name': 'Jane',
    'age': 25,
    'email': 'jane@example.com'
  };
  
  // Encoding Dart Map to JSON
  String jsonString = jsonEncode(user);
  
  print(jsonString);
  // Output: {"name":"Jane","age":25,"email":"jane@example.com"}
}
```

**Using `json_serializable` for Complex Models:**
For complex data models, manual serialization can be cumbersome. The `json_serializable` package automates this process. It requires additional setup, including adding dependencies to your `pubspec.yaml` and creating build files. After setup, you can use it as follows:

1. Define a model with annotations:
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

2. Generate the serialization boilerplate:
Use the build runner command to generate the `user.g.dart` file:
```shell
flutter pub run build_runner build
```

3. Use your model:
```dart
void main() {
  // Parsing JSON to User
  Map userMap = jsonDecode('{"name": "John", "age": 30, "email": "john@example.com"}');
  User user = User.fromJson(userMap);
  
  print('User: ${user.name}, Age: ${user.age}');
  // Output: User: John, Age: 30

  // Converting User back to JSON
  String jsonString = jsonEncode(user.toJson());
  print(jsonString);
  // Output: {"name":"John","age":30,"email":"john@example.com"}
}
```

These examples illustrate basic and advanced JSON interactions in Dart, empowering developers to handle data serialization tasks in their applications seamlessly.
