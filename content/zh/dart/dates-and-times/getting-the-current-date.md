---
title:                "获取当前日期"
date:                  2024-03-08T21:54:53.050730-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

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
