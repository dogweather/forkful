---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:53:44.974512-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A Dart\u63D0\u4F9B\u4E86\u7528\u4E8E\u5904\
  \u7406\u65E5\u671F\u548C\u65F6\u95F4\u7684`DateTime`\u7C7B\uFF0C\u4EE5\u53CA\u7528\
  \u4E8E\u683C\u5F0F\u5316\u7684`intl`\u5305\u3002\u9996\u5148\uFF0C\u786E\u4FDD\u901A\
  \u8FC7\u5728`pubspec.yaml`\u6587\u4EF6\u4E2D\u6DFB\u52A0`intl: ^0.17.0`\uFF08\u6216\
  \u6700\u65B0\u7248\u672C\uFF09\u6765\u5305\u542B`intl`\u5305\u3002 #."
lastmod: '2024-03-13T22:44:47.433908-06:00'
model: gpt-4-0125-preview
summary: "Dart\u63D0\u4F9B\u4E86\u7528\u4E8E\u5904\u7406\u65E5\u671F\u548C\u65F6\u95F4\
  \u7684`DateTime`\u7C7B\uFF0C\u4EE5\u53CA\u7528\u4E8E\u683C\u5F0F\u5316\u7684`intl`\u5305\
  \u3002\u9996\u5148\uFF0C\u786E\u4FDD\u901A\u8FC7\u5728`pubspec.yaml`\u6587\u4EF6\
  \u4E2D\u6DFB\u52A0`intl."
title: "\u5C06\u65E5\u671F\u8F6C\u6362\u4E3A\u5B57\u7B26\u4E32"
weight: 28
---

## 如何操作：
Dart提供了用于处理日期和时间的`DateTime`类，以及用于格式化的`intl`包。首先，确保通过在`pubspec.yaml`文件中添加`intl: ^0.17.0`（或最新版本）来包含`intl`包。

### 使用Dart的核心库
```dart
DateTime now = DateTime.now();
String formattedDate = "${now.year}-${now.month}-${now.day}";
print(formattedDate); // 输出：2023-4-12（例如，这取决于当前日期）
```

此示例直接从`DateTime`的属性构造字符串。

### 使用`intl`包
首先，导入包：

```dart
import 'package:intl/intl.dart';
```

然后，格式化日期：

```dart
DateTime now = DateTime.now();
String formattedDate = DateFormat('yyyy-MM-dd').format(now);
print(formattedDate); // 输出：2023-04-12
```

`intl`包允许轻松地进行更复杂的格式化，包括特定于区域设置的格式：

```dart
String formattedDateLocale = DateFormat.yMMMMd('en_US').format(now);
print(formattedDateLocale); // 输出：April 12, 2023
```

这些示例展示了在Dart中将日期转换并格式化为字符串的简单但强大的方法，无论是使用Dart的核心功能还是利用`intl`包进行更高级的格式化选项。
