---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:54:53.050730-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A Dart \u7684\u6838\u5FC3\u5E93\u901A\u8FC7\
  \ `DateTime` \u7C7B\u63D0\u4F9B\u4E86\u76F4\u63A5\u8BBF\u95EE\u5F53\u524D\u65E5\u671F\
  \u548C\u65F6\u95F4\u7684\u9014\u5F84\u3002\u4EE5\u4E0B\u662F\u83B7\u53D6\u5F53\u524D\
  \u65E5\u671F\u7684\u57FA\u672C\u793A\u4F8B\uFF1A."
lastmod: '2024-03-13T22:44:47.432491-06:00'
model: gpt-4-0125-preview
summary: "Dart \u7684\u6838\u5FC3\u5E93\u901A\u8FC7 `DateTime` \u7C7B\u63D0\u4F9B\u4E86\
  \u76F4\u63A5\u8BBF\u95EE\u5F53\u524D\u65E5\u671F\u548C\u65F6\u95F4\u7684\u9014\u5F84\
  \u3002\u4EE5\u4E0B\u662F\u83B7\u53D6\u5F53\u524D\u65E5\u671F\u7684\u57FA\u672C\u793A\
  \u4F8B\uFF1A."
title: "\u83B7\u53D6\u5F53\u524D\u65E5\u671F"
weight: 29
---

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
