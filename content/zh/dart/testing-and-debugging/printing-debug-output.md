---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:02.428657-07:00
description: "\u5728 Dart \u4E2D\u6253\u5370\u8C03\u8BD5\u8F93\u51FA\u662F\u6307\u5728\
  \u8FD0\u884C\u65F6\u5411\u63A7\u5236\u53F0\u663E\u793A\u4FE1\u606F\uFF0C\u5141\u8BB8\
  \u5F00\u53D1\u4EBA\u5458\u8DDF\u8E2A\u6267\u884C\u6D41\u7A0B\u3001\u8C03\u67E5\u53D8\
  \u91CF\u7684\u72B6\u6001\u6216\u8BC6\u522B\u9519\u8BEF\u7684\u6765\u6E90\u3002\u7A0B\
  \u5E8F\u5458\u901A\u5E38\u4F7F\u7528\u5B83\u6765\u8FDB\u884C\u6545\u969C\u6392\u9664\
  \u548C\u9A8C\u8BC1\u4ED6\u4EEC\u7684\u4EE3\u7801\u662F\u5426\u6309\u9884\u671F\u5DE5\
  \u4F5C\uFF0C\u4ECE\u800C\u4FC3\u8FDB\u4E00\u4E2A\u66F4\u987A\u7545\u548C\u66F4\u9AD8\
  \u6548\u7684\u5F00\u53D1\u8FC7\u7A0B\u3002"
lastmod: '2024-03-13T22:44:47.421764-06:00'
model: gpt-4-0125-preview
summary: "\u5728 Dart \u4E2D\u6253\u5370\u8C03\u8BD5\u8F93\u51FA\u662F\u6307\u5728\
  \u8FD0\u884C\u65F6\u5411\u63A7\u5236\u53F0\u663E\u793A\u4FE1\u606F\uFF0C\u5141\u8BB8\
  \u5F00\u53D1\u4EBA\u5458\u8DDF\u8E2A\u6267\u884C\u6D41\u7A0B\u3001\u8C03\u67E5\u53D8\
  \u91CF\u7684\u72B6\u6001\u6216\u8BC6\u522B\u9519\u8BEF\u7684\u6765\u6E90\u3002\u7A0B\
  \u5E8F\u5458\u901A\u5E38\u4F7F\u7528\u5B83\u6765\u8FDB\u884C\u6545\u969C\u6392\u9664\
  \u548C\u9A8C\u8BC1\u4ED6\u4EEC\u7684\u4EE3\u7801\u662F\u5426\u6309\u9884\u671F\u5DE5\
  \u4F5C\uFF0C\u4ECE\u800C\u4FC3\u8FDB\u4E00\u4E2A\u66F4\u987A\u7545\u548C\u66F4\u9AD8\
  \u6548\u7684\u5F00\u53D1\u8FC7\u7A0B\u3002."
title: "\u6253\u5370\u8C03\u8BD5\u8F93\u51FA"
weight: 33
---

## 什么 & 为什么？

在 Dart 中打印调试输出是指在运行时向控制台显示信息，允许开发人员跟踪执行流程、调查变量的状态或识别错误的来源。程序员通常使用它来进行故障排除和验证他们的代码是否按预期工作，从而促进一个更顺畅和更高效的开发过程。

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
