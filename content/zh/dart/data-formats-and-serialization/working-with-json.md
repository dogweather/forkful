---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:57:36.154109-07:00
description: "\u4F7F\u7528 JSON\uFF08JavaScript \u5BF9\u8C61\u8868\u793A\u6CD5\uFF09\
  \u5305\u62EC\u5C06 JSON \u6570\u636E\u4ECE\u5B57\u7B26\u4E32\u89E3\u6790\u6210 Dart\
  \ \u5BF9\u8C61\uFF0C\u53CD\u4E4B\u4EA6\u7136\uFF0C\u8FD9\u662F\u7F51\u9875\u548C\
  \u5E94\u7528\u5F00\u53D1\u4E2D\u5E38\u89C1\u7684\u6570\u636E\u4EA4\u6362\u4EFB\u52A1\
  \u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\u662F\u4E3A\u4E86\u6709\u6548\u5730\u5904\
  \u7406\u6765\u81EA API\u3001\u914D\u7F6E\u6216\u5E94\u7528\u5185\u7EC4\u4EF6\u95F4\
  \u901A\u4FE1\u7684\u6570\u636E\u3002"
lastmod: '2024-03-13T22:44:47.446949-06:00'
model: gpt-4-0125-preview
summary: "\u4F7F\u7528 JSON\uFF08JavaScript \u5BF9\u8C61\u8868\u793A\u6CD5\uFF09\u5305\
  \u62EC\u5C06 JSON \u6570\u636E\u4ECE\u5B57\u7B26\u4E32\u89E3\u6790\u6210 Dart \u5BF9\
  \u8C61\uFF0C\u53CD\u4E4B\u4EA6\u7136\uFF0C\u8FD9\u662F\u7F51\u9875\u548C\u5E94\u7528\
  \u5F00\u53D1\u4E2D\u5E38\u89C1\u7684\u6570\u636E\u4EA4\u6362\u4EFB\u52A1\u3002\u7A0B\
  \u5E8F\u5458\u8FD9\u6837\u505A\u662F\u4E3A\u4E86\u6709\u6548\u5730\u5904\u7406\u6765\
  \u81EA API\u3001\u914D\u7F6E\u6216\u5E94\u7528\u5185\u7EC4\u4EF6\u95F4\u901A\u4FE1\
  \u7684\u6570\u636E\u3002."
title: "\u4F7F\u7528JSON\u7684\u5DE5\u4F5C\u65B9\u5F0F"
weight: 38
---

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
