---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:24.682587-07:00
description: "\u600E\u4E48\u505A\uFF1A Dart \u7684\u6838\u5FC3\u5E93\u901A\u8FC7 `DateTime`\
  \ \u7C7B\u7B80\u5316\u4E86\u65E5\u671F\u89E3\u6790\u3002\u5BF9\u4E8E\u76F4\u63A5\
  \u7684\u60C5\u51B5\uFF0C\u5982\u679C\u4F60\u77E5\u9053\u65E5\u671F\u5B57\u7B26\u4E32\
  \u7684\u683C\u5F0F\uFF0C\u53EF\u4EE5\u4F7F\u7528 `DateTime.parse()` \u65B9\u6CD5\
  \u3002\u7136\u800C\uFF0C\u5BF9\u4E8E\u66F4\u590D\u6742\u7684\u573A\u666F\u6216\u5904\
  \u7406\u591A\u79CD\u683C\u5F0F\u65F6\uFF0C`intl` \u5305\uFF0C\u7279\u522B\u662F\
  \ `DateFormat` \u7C7B\uFF0C\u53D8\u5F97\u975E\u5E38\u6709\u4EF7\u503C\u3002 #."
lastmod: '2024-03-13T22:44:47.431105-06:00'
model: gpt-4-0125-preview
summary: "Dart \u7684\u6838\u5FC3\u5E93\u901A\u8FC7 `DateTime` \u7C7B\u7B80\u5316\u4E86\
  \u65E5\u671F\u89E3\u6790\u3002\u5BF9\u4E8E\u76F4\u63A5\u7684\u60C5\u51B5\uFF0C\u5982\
  \u679C\u4F60\u77E5\u9053\u65E5\u671F\u5B57\u7B26\u4E32\u7684\u683C\u5F0F\uFF0C\u53EF\
  \u4EE5\u4F7F\u7528 `DateTime.parse()` \u65B9\u6CD5\u3002\u7136\u800C\uFF0C\u5BF9\
  \u4E8E\u66F4\u590D\u6742\u7684\u573A\u666F\u6216\u5904\u7406\u591A\u79CD\u683C\u5F0F\
  \u65F6\uFF0C`intl` \u5305\uFF0C\u7279\u522B\u662F `DateFormat` \u7C7B\uFF0C\u53D8\
  \u5F97\u975E\u5E38\u6709\u4EF7\u503C."
title: "\u4ECE\u5B57\u7B26\u4E32\u89E3\u6790\u65E5\u671F"
weight: 30
---

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
