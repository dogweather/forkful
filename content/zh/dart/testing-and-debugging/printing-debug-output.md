---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:02.428657-07:00
description: "\u5982\u4F55\u8FDB\u884C\uFF1A \u5728 Dart \u4E2D\uFF0C\u60A8\u53EF\u4EE5\
  \u4F7F\u7528 `print()` \u51FD\u6570\u6253\u5370\u8C03\u8BD5\u8F93\u51FA\u3002\u4EE5\
  \u4E0B\u662F\u5982\u4F55\u8F93\u51FA\u7B80\u5355\u6D88\u606F\u548C\u53D8\u91CF\u503C\
  \u7684\u65B9\u6CD5\uFF1A."
lastmod: '2024-04-05T22:38:46.584591-06:00'
model: gpt-4-0125-preview
summary: "\u5982\u4F55\u8FDB\u884C\uFF1A \u5728 Dart \u4E2D\uFF0C\u60A8\u53EF\u4EE5\
  \u4F7F\u7528 `print()` \u51FD\u6570\u6253\u5370\u8C03\u8BD5\u8F93\u51FA\u3002\u4EE5\
  \u4E0B\u662F\u5982\u4F55\u8F93\u51FA\u7B80\u5355\u6D88\u606F\u548C\u53D8\u91CF\u503C\
  \u7684\u65B9\u6CD5\uFF1A."
title: "\u6253\u5370\u8C03\u8BD5\u8F93\u51FA"
weight: 33
---

## 如何进行：
在 Dart 中，您可以使用 `print()` 函数打印调试输出。以下是如何输出简单消息和变量值的方法：

```dart
void main() {
  String greeting = "Hello, Dart!";
  print(greeting); // 打印：Hello, Dart!

  int number = 42;
  print('The number is $number.'); // 打印：The number is 42.
}
```

对于如列表或对象等结构化数据，Dart 的 `toString()` 方法可能不提供足够的细节。在这些情况下，您可以使用 Dart 的 `dart:convert` 库中的 `jsonEncode` 函数将数据转换为 JSON 字符串，以便于阅读输出：

```dart
import 'dart:convert';

void main() {
  var user = {
    'name': 'John Doe',
    'age': 30,
    'emails': ['john.doe@example.com', 'john@example.com'],
  };

  print(jsonEncode(user));
  // 打印：{"name":"John Doe","age":30,"emails":["john.doe@example.com","john@example.com"]}
}
```

当需要更复杂的调试能力时，如使用不同重要性级别（信息，警告，错误）的日志记录，您可以使用第三方库如 `logger`。以下是如何使用它的方法：

1. 将 `logger` 添加到您的 `pubspec.yaml`：

```yaml
dependencies:
  logger: ^1.0.0
```

2. 在您的 Dart 代码中使用 `logger`：

```dart
import 'package:logger/logger.dart';

var logger = Logger();

void main() {
  logger.d("This is a debug message");
  logger.w("This is a warning message");
  logger.e("This is an error message");
}
```

输出将更具信息性，显示消息的级别和消息本身，使得区分不同种类的日志消息变得更加容易。
