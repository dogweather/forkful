---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:54:53.050730-07:00
description: "\u5728 Dart \u4E2D\u83B7\u53D6\u5F53\u524D\u65E5\u671F\u6D89\u53CA\u5230\
  \u67E5\u8BE2\u7CFB\u7EDF\u4EE5\u83B7\u5F97\u5F53\u524D\u7684\u65E5\u671F\u548C\u65F6\
  \u95F4\u3002\u8FD9\u4E2A\u529F\u80FD\u901A\u5E38\u5728\u5E94\u7528\u7A0B\u5E8F\u4E2D\
  \u7528\u4E8E\u5982\u65F6\u95F4\u6233\u4E8B\u4EF6\u3001\u5411\u7528\u6237\u663E\u793A\
  \u5F53\u524D\u65E5\u671F\u6216\u8BA1\u7B97\u6301\u7EED\u65F6\u95F4\u7B49\u7279\u6027\
  \u3002\u4E86\u89E3\u5982\u4F55\u6709\u6548\u68C0\u7D22\u548C\u64CD\u4F5C\u5F53\u524D\
  \u65E5\u671F\u5BF9\u4E8E\u8BA1\u5212\u5B89\u6392\u3001\u65E5\u5FD7\u8BB0\u5F55\u548C\
  \u65F6\u95F4\u654F\u611F\u7684\u7279\u6027\u800C\u8A00\u662F\u57FA\u7840\u3002"
lastmod: '2024-03-13T22:44:47.432491-06:00'
model: gpt-4-0125-preview
summary: "\u5728 Dart \u4E2D\u83B7\u53D6\u5F53\u524D\u65E5\u671F\u6D89\u53CA\u5230\
  \u67E5\u8BE2\u7CFB\u7EDF\u4EE5\u83B7\u5F97\u5F53\u524D\u7684\u65E5\u671F\u548C\u65F6\
  \u95F4\u3002\u8FD9\u4E2A\u529F\u80FD\u901A\u5E38\u5728\u5E94\u7528\u7A0B\u5E8F\u4E2D\
  \u7528\u4E8E\u5982\u65F6\u95F4\u6233\u4E8B\u4EF6\u3001\u5411\u7528\u6237\u663E\u793A\
  \u5F53\u524D\u65E5\u671F\u6216\u8BA1\u7B97\u6301\u7EED\u65F6\u95F4\u7B49\u7279\u6027\
  \u3002\u4E86\u89E3\u5982\u4F55\u6709\u6548\u68C0\u7D22\u548C\u64CD\u4F5C\u5F53\u524D\
  \u65E5\u671F\u5BF9\u4E8E\u8BA1\u5212\u5B89\u6392\u3001\u65E5\u5FD7\u8BB0\u5F55\u548C\
  \u65F6\u95F4\u654F\u611F\u7684\u7279\u6027\u800C\u8A00\u662F\u57FA\u7840\u3002."
title: "\u83B7\u53D6\u5F53\u524D\u65E5\u671F"
weight: 29
---

## 什么和为什么？
在 Dart 中获取当前日期涉及到查询系统以获得当前的日期和时间。这个功能通常在应用程序中用于如时间戳事件、向用户显示当前日期或计算持续时间等特性。了解如何有效检索和操作当前日期对于计划安排、日志记录和时间敏感的特性而言是基础。

## 如何操作：
Dart 的核心库通过 `DateTime` 类提供了直接访问当前日期和时间的途径。以下是获取当前日期的基本示例：

```dart
void main() {
  DateTime now = DateTime.now();
  print(now); // 示例输出：2023-04-12 10:00:00.000
}
```

如果您只需要日期部分（年、月、日），您可以格式化 `DateTime` 对象：

```dart
void main() {
  DateTime now = DateTime.now();
  String formattedDate = "${now.year}-${now.month}-${now.day}";
  print(formattedDate); // 示例输出：2023-04-12
}
```

Dart 并不包含用于更复杂日期格式化的内建库，但您可以使用 `intl` 包达到此目的。首先，将该包添加到您的 `pubspec.yaml` 中：

```yaml
dependencies:
  intl: ^0.17.0
```

然后，您可以轻松格式化日期：

```dart
import 'package:intl/intl.dart';

void main() {
  DateTime now = DateTime.now();
  String formattedDate = DateFormat('yyyy-MM-dd').format(now);
  print(formattedDate); // 示例输出：2023-04-12
}
```

要获得更高级的格式化选项，请探索 `intl` 包提供的 `DateFormat` 类，它支持广泛的模式和地区设置。
