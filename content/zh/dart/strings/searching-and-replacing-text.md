---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:24.658525-07:00
description: "\u600E\u4E48\u505A\uFF1A Dart \u901A\u8FC7\u5176 `String` \u7C7B\u76F4\
  \u63A5\u63D0\u4F9B\u4E86\u641C\u7D22\u548C\u66FF\u6362\u6587\u672C\u7684\u5F3A\u5927\
  \u65B9\u6CD5\uFF0C\u65E0\u9700\u5916\u90E8\u5E93\u3002\u4EE5\u4E0B\u662F\u4F60\u53EF\
  \u4EE5\u5982\u4F55\u505A\u5230\u8FD9\u4E00\u70B9\uFF1A."
lastmod: '2024-04-05T22:38:46.562538-06:00'
model: gpt-4-0125-preview
summary: "\u600E\u4E48\u505A\uFF1A Dart \u901A\u8FC7\u5176 `String` \u7C7B\u76F4\u63A5\
  \u63D0\u4F9B\u4E86\u641C\u7D22\u548C\u66FF\u6362\u6587\u672C\u7684\u5F3A\u5927\u65B9\
  \u6CD5\uFF0C\u65E0\u9700\u5916\u90E8\u5E93\u3002\u4EE5\u4E0B\u662F\u4F60\u53EF\u4EE5\
  \u5982\u4F55\u505A\u5230\u8FD9\u4E00\u70B9\uFF1A."
title: "\u641C\u7D22\u548C\u66FF\u6362\u6587\u672C"
weight: 10
---

## 怎么做：
Dart 通过其 `String` 类直接提供了搜索和替换文本的强大方法，无需外部库。以下是你可以如何做到这一点：

### 基本搜索和替换
要搜索子字符串并将其替换为另一个字符串，你可以使用 `replaceAll`：

```dart
String sampleText = "Hello, Dart! Dart is great.";
String modifiedText = sampleText.replaceAll("Dart", "Flutter");
print(modifiedText); // 输出：Hello, Flutter! Flutter is great.
```

### 使用正则表达式
对于更复杂的搜索和替换需求，Dart 通过 `RegExp` 类使用正则表达式。这允许在字符串中进行模式匹配和替换：

```dart
String sampleText = "Dart 2023, Flutter 2023";
String modifiedText = sampleText.replaceAll(RegExp(r'\d+'), "2024");
print(modifiedText); // 输出：Dart 2024, Flutter 2024
```

此示例查找字符串中所有一个或多个数字（`\d+`）的实例，并将它们替换为“2024”。

### 不区分大小写的搜索
要执行不区分大小写的搜索，你可以修改 `RegExp` 构造函数以忽略大小写：

```dart
String sampleText = "Welcome to Dart, the programming language.";
String modifiedText = sampleText.replaceAll(RegExp(r'dart', caseSensitive: false), "Flutter");
print(modifiedText); // 输出：Welcome to Flutter, the programming language.
```

### 使用函数替换
对于基于匹配项本身的动态替换，Dart 允许将函数传递给 `replaceAllMapped`。此函数可以对匹配的序列执行操作或计算：

```dart
String sampleText = "Increment 5 by 1 to get 6.";
String incrementedText = sampleText.replaceAllMapped(RegExp(r'\d+'), (Match m) => (int.parse(m[0]!) + 1).toString());
print(incrementedText); // 输出：Increment 6 by 1 to get 7.
```

这将每个数字序列替换为其增量值。每个匹配项都被解析为一个整数，增加，然后转换回字符串进行替换。

Dart 的字符串操作能力，特别是对于搜索和替换文本，使其成为一个强大的工具，用于处理和准备应用程序中的数据。无论是使用简单的字符串替换还是利用正则表达式的力量，Dart 都提供了有效文本操作所需的灵活性和性能。
