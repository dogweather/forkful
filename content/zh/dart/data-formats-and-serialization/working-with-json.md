---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:57:36.154109-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A Dart \u901A\u8FC7 `dart:convert` \u5E93\
  \u4E3A JSON \u63D0\u4F9B\u4E86\u5185\u7F6E\u652F\u6301\uFF0C\u4F7F\u5F97\u7F16\u7801\
  \u548C\u89E3\u7801 JSON \u53D8\u5F97\u7B80\u5355\u76F4\u63A5\u3002\u4E0B\u9762\u7684\
  \u4F8B\u5B50\u5C55\u793A\u4E86\u57FA\u672C\u64CD\u4F5C\uFF1A **\u5C06 JSON \u5B57\
  \u7B26\u4E32\u89E3\u6790\u4E3A Dart \u5BF9\u8C61\uFF1A**."
lastmod: '2024-04-05T22:38:46.606923-06:00'
model: gpt-4-0125-preview
summary: "convert` \u5E93\u4E3A JSON \u63D0\u4F9B\u4E86\u5185\u7F6E\u652F\u6301\uFF0C\
  \u4F7F\u5F97\u7F16\u7801\u548C\u89E3\u7801 JSON \u53D8\u5F97\u7B80\u5355\u76F4\u63A5\
  \u3002\u4E0B\u9762\u7684\u4F8B\u5B50\u5C55\u793A\u4E86\u57FA\u672C\u64CD\u4F5C\uFF1A\
  \ **\u5C06 JSON \u5B57\u7B26\u4E32\u89E3\u6790\u4E3A Dart \u5BF9\u8C61\uFF1A**."
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
