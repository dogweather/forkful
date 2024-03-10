---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:57:13.190879-07:00
description: "\u5904\u7406 CSV\uFF08\u9017\u53F7\u5206\u9694\u503C\uFF09\u6587\u4EF6\
  \u6D89\u53CA\u5230\u89E3\u6790\u548C\u751F\u6210\u6BCF\u884C\u90FD\u7531\u9017\u53F7\
  \u5206\u9694\u7684\u503C\u7684\u6587\u672C\u6587\u4EF6\u3002\u7A0B\u5E8F\u5458\u8FD9\
  \u6837\u505A\u662F\u4E3A\u4E86\u4F7F\u4E0D\u540C\u5E94\u7528\u7A0B\u5E8F\u4E4B\u95F4\
  \u7684\u6570\u636E\u4EA4\u6362\u6210\u4E3A\u53EF\u80FD\uFF0C\u6216\u8005\u4E3A\u4E86\
  \u4FBF\u4E8E\u4EE5\u8F7B\u91CF\u7EA7\u3001\u53EF\u8BFB\u7684\u683C\u5F0F\u5B58\u50A8\
  \u6570\u636E\u3002"
lastmod: '2024-03-09T21:06:11.760821-07:00'
model: gpt-4-0125-preview
summary: "\u5904\u7406 CSV\uFF08\u9017\u53F7\u5206\u9694\u503C\uFF09\u6587\u4EF6\u6D89\
  \u53CA\u5230\u89E3\u6790\u548C\u751F\u6210\u6BCF\u884C\u90FD\u7531\u9017\u53F7\u5206\
  \u9694\u7684\u503C\u7684\u6587\u672C\u6587\u4EF6\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\
  \u505A\u662F\u4E3A\u4E86\u4F7F\u4E0D\u540C\u5E94\u7528\u7A0B\u5E8F\u4E4B\u95F4\u7684\
  \u6570\u636E\u4EA4\u6362\u6210\u4E3A\u53EF\u80FD\uFF0C\u6216\u8005\u4E3A\u4E86\u4FBF\
  \u4E8E\u4EE5\u8F7B\u91CF\u7EA7\u3001\u53EF\u8BFB\u7684\u683C\u5F0F\u5B58\u50A8\u6570\
  \u636E\u3002"
title: "\u64CD\u4F5CCSV"
---

{{< edit_this_page >}}

## 什么 & 为什么?

处理 CSV（逗号分隔值）文件涉及到解析和生成每行都由逗号分隔的值的文本文件。程序员这样做是为了使不同应用程序之间的数据交换成为可能，或者为了便于以轻量级、可读的格式存储数据。

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
