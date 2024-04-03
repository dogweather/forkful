---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:24.682587-07:00
description: "\u5728 Dart \u4E2D\u89E3\u6790\u5B57\u7B26\u4E32\u4E2D\u7684\u65E5\u671F\
  \u6D89\u53CA\u5C06\u65E5\u671F\u548C\u65F6\u95F4\u7684\u6587\u672C\u8868\u793A\u8F6C\
  \u6362\u4E3A `DateTime` \u5BF9\u8C61\u3002\u8FD9\u4E00\u64CD\u4F5C\u5BF9\u4E8E\u5904\
  \u7406\u8C03\u5EA6\u3001\u6570\u636E\u5206\u6790\u6216\u4EFB\u4F55\u9700\u8981\u65E5\
  \u671F\u64CD\u4F5C\u7684\u529F\u80FD\u81F3\u5173\u91CD\u8981\uFF0C\u786E\u4FDD\u7A0B\
  \u5E8F\u6B63\u786E\u7406\u89E3\u548C\u5904\u7406\u4E0E\u65E5\u671F\u76F8\u5173\u7684\
  \u6570\u636E\u3002"
lastmod: '2024-03-13T22:44:47.431105-06:00'
model: gpt-4-0125-preview
summary: "\u5728 Dart \u4E2D\u89E3\u6790\u5B57\u7B26\u4E32\u4E2D\u7684\u65E5\u671F\
  \u6D89\u53CA\u5C06\u65E5\u671F\u548C\u65F6\u95F4\u7684\u6587\u672C\u8868\u793A\u8F6C\
  \u6362\u4E3A `DateTime` \u5BF9\u8C61\u3002\u8FD9\u4E00\u64CD\u4F5C\u5BF9\u4E8E\u5904\
  \u7406\u8C03\u5EA6\u3001\u6570\u636E\u5206\u6790\u6216\u4EFB\u4F55\u9700\u8981\u65E5\
  \u671F\u64CD\u4F5C\u7684\u529F\u80FD\u81F3\u5173\u91CD\u8981\uFF0C\u786E\u4FDD\u7A0B\
  \u5E8F\u6B63\u786E\u7406\u89E3\u548C\u5904\u7406\u4E0E\u65E5\u671F\u76F8\u5173\u7684\
  \u6570\u636E\u3002."
title: "\u4ECE\u5B57\u7B26\u4E32\u89E3\u6790\u65E5\u671F"
weight: 30
---

## 是什么 & 为什么？
在 Dart 中解析字符串中的日期涉及将日期和时间的文本表示转换为 `DateTime` 对象。这一操作对于处理调度、数据分析或任何需要日期操作的功能至关重要，确保程序正确理解和处理与日期相关的数据。

## 怎么做：
Dart 的核心库通过 `DateTime` 类简化了日期解析。对于直接的情况，如果你知道日期字符串的格式，可以使用 `DateTime.parse()` 方法。然而，对于更复杂的场景或处理多种格式时，`intl` 包，特别是 `DateFormat` 类，变得非常有价值。

### 使用 Dart 核心库：
```dart
void main() {
  // 使用 DateTime.parse()
  var dateString = "2023-10-31";
  var parsedDate = DateTime.parse(dateString);
  
  print(parsedDate); // 2023-10-31 00:00:00.000
}
```

### 使用 `intl` 包：
首先，在你的 `pubspec.yaml` 文件中添加 `intl` 包：
```yaml
dependencies:
  intl: ^0.17.0
```
然后，导入包并使用 `DateFormat` 进行解析：
```dart
import 'package:intl/intl.dart';

void main() {
  var dateString = "October 31, 2023";
  var dateFormat = DateFormat("MMMM dd, yyyy");
  var parsedDate = dateFormat.parse(dateString);
  
  print(parsedDate); // 2023-10-31 00:00:00.000
}
```
`intl` 包提供了强大的日期解析选项，允许无缝处理各种国际日期格式。
