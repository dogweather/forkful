---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:57:13.190879-07:00
description: "\u5982\u4F55\u64CD\u4F5C: \u5728 Dart \u4E2D\u5904\u7406 CSV \u6587\u4EF6\
  \uFF0C\u4F60\u901A\u5E38\u8981\u4E48\u624B\u52A8\u5904\u7406\u6587\u672C\uFF0C\u8981\
  \u4E48\u4F7F\u7528\u7B2C\u4E09\u65B9\u5E93\u6765\u7B80\u5316\u4EFB\u52A1\u3002\u8FD9\
  \u91CC\uFF0C\u6211\u4EEC\u5C06\u770B\u770B\u4E24\u79CD\u65B9\u6CD5\u3002"
lastmod: '2024-04-05T21:53:47.775455-06:00'
model: gpt-4-0125-preview
summary: "\u5728 Dart \u4E2D\u5904\u7406 CSV \u6587\u4EF6\uFF0C\u4F60\u901A\u5E38\u8981\
  \u4E48\u624B\u52A8\u5904\u7406\u6587\u672C\uFF0C\u8981\u4E48\u4F7F\u7528\u7B2C\u4E09\
  \u65B9\u5E93\u6765\u7B80\u5316\u4EFB\u52A1\u3002\u8FD9\u91CC\uFF0C\u6211\u4EEC\u5C06\
  \u770B\u770B\u4E24\u79CD\u65B9\u6CD5\u3002"
title: "\u64CD\u4F5CCSV"
weight: 37
---

## 如何操作:
在 Dart 中处理 CSV 文件，你通常要么手动处理文本，要么使用第三方库来简化任务。这里，我们将看看两种方法。

### 手动解析 CSV
如果你的需求很简单，你可能会选择手动解析 CSV 字符串。这可以通过使用 Dart 的核心字符串处理功能来实现：

```dart
void main() {
  // 示例 CSV 数据
  String csvData = "Name,Age,Email\nJohn Doe,30,john@example.com\nJane Smith,25,jane@example.com";
  
  // 将 CSV 数据分割成行
  List<String> lines = csvData.split('\n');
  
  // 解析每行
  List<Map<String, String>> data = [];
  List<String> headers = lines.first.split(',');
  
  for (var i = 1; i < lines.length; i++) {
    List<String> row = lines[i].split(',');
    Map<String, String> record = {};
    for (var j = 0; j < headers.length; j++) {
      record[headers[j]] = row[j];
    }
    data.add(record);
  }
  
  // 输出解析的数据
  print(data);
}

// 示例输出：
// [{Name: John Doe, Age: 30, Email: john@example.com}, {Name: Jane Smith, Age: 25, Email: jane@example.com}]
```

### 使用第三方库：`csv`
对于更复杂的情况或为了简化代码，你可以使用像 `csv` 这样的流行第三方库。首先，在你的 `pubspec.yaml` 文件的 `dependencies` 下添加 `csv: ^5.0.0`（或最新版本）以将其加入项目。然后按以下方式使用它：

```dart
import 'package:csv/csv.dart';

void main() {
  String csvData = "Name,Age,Email\nJohn Doe,30,john@example.com\nJane Smith,25,jane@example.com";
  
  // 使用 CsvToListConverter 解析 CSV 数据
  List<List<dynamic>> listData = const CsvToListConverter().convert(csvData);
  
  // 第一个列表项包含头部
  List<String> headers = listData.first.map((item) => item.toString()).toList();
  
  // 在进一步处理之前移除头部行
  listData.removeAt(0);
  
  // 转换为 List<Map<String, dynamic>> 以获得更结构化的格式
  List<Map<String, dynamic>> mappedData = listData.map((list) {
    Map<String, dynamic> map = {};
    for (int i = 0; i < headers.length; i++) {
      map[headers[i]] = list[i];
    }
    return map;
  }).toList();
  
  // 输出映射的数据
  print(mappedData);
}

// 示例输出：
// [{Name: John Doe, Age: 30, Email: john@example.com}, {Name: Jane Smith, Age: 25, Email: jane@example.com}]
```

这两种方法都演示了如何处理 CSV 数据：第一种手动解析，用于学习目的或处理非常简单的 CSV 结构；第二种，则通过利用强大的库来简化解析，并能处理 CSV 格式的各种复杂性。
