---
title:                "使用JSON的工作方式"
date:                  2024-03-08T21:57:36.154109-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## 什么与为什么？

使用 JSON（JavaScript 对象表示法）包括将 JSON 数据从字符串解析成 Dart 对象，反之亦然，这是网页和应用开发中常见的数据交换任务。程序员这样做是为了有效地处理来自 API、配置或应用内组件间通信的数据。

## 如何操作：

Dart 通过 `dart:convert` 库为 JSON 提供了内置支持，使得编码和解码 JSON 变得简单直接。下面的例子展示了基本操作：

**将 JSON 字符串解析为 Dart 对象：**
```dart
import 'dart:convert';

void main() {
  // 示例 JSON 字符串
  String jsonString = '{"name": "John", "age": 30, "email": "john@example.com"}';
  
  // 将 JSON 解码为 Dart Map
  Map<String, dynamic> user = jsonDecode(jsonString);
  
  print('Hello, ${user['name']}! You are ${user['age']} years old.');
  // 输出：Hello, John! You are 30 years old.
}
```

**将 Dart 对象编码为 JSON 字符串：**
```dart
import 'dart:convert';

void main() {
  // 示例 Dart 对象
  Map<String, dynamic> user = {
    'name': 'Jane',
    'age': 25,
    'email': 'jane@example.com'
  };
  
  // 将 Dart Map 编码为 JSON
  String jsonString = jsonEncode(user);
  
  print(jsonString);
  // 输出：{"name":"Jane","age":25,"email":"jane@example.com"}
}
```

**使用 `json_serializable` 处理复杂模型：**
对于复杂的数据模型，手动序列化可能会很麻烦。`json_serializable` 包可自动化此过程。它需要额外的设置，包括在你的 `pubspec.yaml` 中添加依赖并创建构建文件。设置后，就可以如下使用：

1. 使用注解定义模型：
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

2. 生成序列化样板文件：
使用 build runner 命令生成 `user.g.dart` 文件：
```shell
flutter pub run build_runner build
```

3. 使用你的模型：
```dart
void main() {
  // 将 JSON 解析为 User
  Map userMap = jsonDecode('{"name": "John", "age": 30, "email": "john@example.com"}');
  User user = User.fromJson(userMap);
  
  print('User: ${user.name}, Age: ${user.age}');
  // 输出：User: John, Age: 30

  // 将 User 转换回 JSON
  String jsonString = jsonEncode(user.toJson());
  print(jsonString);
  // 输出：{"name":"John","age":30,"email":"john@example.com"}
}
```

这些示例展示了 Dart 中基础与高级的 JSON 交互操作，使开发者能够在其应用中无缝处理数据序列化任务。
