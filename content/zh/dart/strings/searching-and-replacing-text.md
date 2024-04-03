---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:24.658525-07:00
description: "\u5728 Dart \u4E2D\u641C\u7D22\u548C\u66FF\u6362\u6587\u672C\u6D89\u53CA\
  \u68C0\u67E5\u5B57\u7B26\u4E32\u4EE5\u627E\u5230\u67D0\u4E9B\u6A21\u5F0F\u6216\u5B57\
  \u7B26\u5E8F\u5217\uFF0C\u5E76\u5C06\u5B83\u4EEC\u66FF\u6362\u4E3A\u65B0\u5185\u5BB9\
  \u3002\u8FD9\u79CD\u64CD\u4F5C\u662F\u6570\u636E\u9A8C\u8BC1\u3001\u683C\u5F0F\u5316\
  \u8F93\u51FA\u3001\u89E3\u6790\u7528\u6237\u8F93\u5165\u6216\u751A\u81F3\u64CD\u4F5C\
  \ URL \u548C\u6587\u4EF6\u8DEF\u5F84\u7B49\u4EFB\u52A1\u7684\u57FA\u7840\uFF0C\u4F7F\
  \u5E94\u7528\u7A0B\u5E8F\u66F4\u52A0\u52A8\u6001\uFF0C\u66F4\u80FD\u54CD\u5E94\u7528\
  \u6237\u9700\u6C42\u3002"
lastmod: '2024-03-13T22:44:47.398435-06:00'
model: gpt-4-0125-preview
summary: "\u5728 Dart \u4E2D\u641C\u7D22\u548C\u66FF\u6362\u6587\u672C\u6D89\u53CA\
  \u68C0\u67E5\u5B57\u7B26\u4E32\u4EE5\u627E\u5230\u67D0\u4E9B\u6A21\u5F0F\u6216\u5B57\
  \u7B26\u5E8F\u5217\uFF0C\u5E76\u5C06\u5B83\u4EEC\u66FF\u6362\u4E3A\u65B0\u5185\u5BB9\
  \u3002\u8FD9\u79CD\u64CD\u4F5C\u662F\u6570\u636E\u9A8C\u8BC1\u3001\u683C\u5F0F\u5316\
  \u8F93\u51FA\u3001\u89E3\u6790\u7528\u6237\u8F93\u5165\u6216\u751A\u81F3\u64CD\u4F5C\
  \ URL \u548C\u6587\u4EF6\u8DEF\u5F84\u7B49\u4EFB\u52A1\u7684\u57FA\u7840\uFF0C\u4F7F\
  \u5E94\u7528\u7A0B\u5E8F\u66F4\u52A0\u52A8\u6001\uFF0C\u66F4\u80FD\u54CD\u5E94\u7528\
  \u6237\u9700\u6C42\u3002."
title: "\u641C\u7D22\u548C\u66FF\u6362\u6587\u672C"
weight: 10
---

## 什么 & 为什么？

在 Dart 中搜索和替换文本涉及检查字符串以找到某些模式或字符序列，并将它们替换为新内容。这种操作是数据验证、格式化输出、解析用户输入或甚至操作 URL 和文件路径等任务的基础，使应用程序更加动态，更能响应用户需求。

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
