---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:39.422397-07:00
description: "\u5728 Dart \u4E2D\u8BFB\u53D6\u6587\u672C\u6587\u4EF6\u6D89\u53CA\u8BBF\
  \u95EE\u548C\u68C0\u7D22\u5B58\u50A8\u5728\u6587\u4EF6\u7CFB\u7EDF\u4E0A\u7684\u6587\
  \u4EF6\u4E2D\u7684\u6570\u636E\u3002\u7A0B\u5E8F\u5458\u8FDB\u884C\u8FD9\u4E00\u64CD\
  \u4F5C\u662F\u4E3A\u4E86\u5904\u7406\u8F93\u5165\u6570\u636E\u3001\u914D\u7F6E\u8BBE\
  \u7F6E\u6216\u8BFB\u53D6\u6570\u636E\u96C6\uFF0C\u8FD9\u4F7F\u5F97\u5B83\u6210\u4E3A\
  \u4ECE\u7B80\u5355\u811A\u672C\u5230\u590D\u6742\u5E94\u7528\u7A0B\u5E8F\u7684\u8BB8\
  \u591A\u5E94\u7528\u4E2D\u7684\u4E00\u4E2A\u57FA\u672C\u64CD\u4F5C\u3002"
lastmod: '2024-03-13T22:44:47.441816-06:00'
model: gpt-4-0125-preview
summary: "\u5728 Dart \u4E2D\u8BFB\u53D6\u6587\u672C\u6587\u4EF6\u6D89\u53CA\u8BBF\
  \u95EE\u548C\u68C0\u7D22\u5B58\u50A8\u5728\u6587\u4EF6\u7CFB\u7EDF\u4E0A\u7684\u6587\
  \u4EF6\u4E2D\u7684\u6570\u636E\u3002\u7A0B\u5E8F\u5458\u8FDB\u884C\u8FD9\u4E00\u64CD\
  \u4F5C\u662F\u4E3A\u4E86\u5904\u7406\u8F93\u5165\u6570\u636E\u3001\u914D\u7F6E\u8BBE\
  \u7F6E\u6216\u8BFB\u53D6\u6570\u636E\u96C6\uFF0C\u8FD9\u4F7F\u5F97\u5B83\u6210\u4E3A\
  \u4ECE\u7B80\u5355\u811A\u672C\u5230\u590D\u6742\u5E94\u7528\u7A0B\u5E8F\u7684\u8BB8\
  \u591A\u5E94\u7528\u4E2D\u7684\u4E00\u4E2A\u57FA\u672C\u64CD\u4F5C\u3002."
title: "\u8BFB\u53D6\u6587\u672C\u6587\u4EF6"
weight: 22
---

## 什么 & 为什么？

在 Dart 中读取文本文件涉及访问和检索存储在文件系统上的文件中的数据。程序员进行这一操作是为了处理输入数据、配置设置或读取数据集，这使得它成为从简单脚本到复杂应用程序的许多应用中的一个基本操作。

## 如何操作：

Dart 的核心库 `dart:io` 提供了同步或异步读取文本文件所需的功能。以下是两种方法的操作指南。

**同步方式：**

```dart
import 'dart:io';

void main() {
  var fileName = "path/to/your/textfile.txt";
  var file = File(fileName);

  // 同步读取文件
  var contents;
  try {
    contents = file.readAsStringSync();
    print(contents);
  } catch (e) {
    print('读取文件错误: $e');
  }
}
```

**异步方式：**

为了避免在读取文件时阻塞程序，特别是对于大文件或需要响应性应用的场景：

```dart
import 'dart:io';

void main() async {
  var fileName = "path/to/your/textfile.txt";
  var file = File(fileName);

  try {
    String contents = await file.readAsString();
    print(contents);
  } catch (e) {
    print('读取文件错误: $e');
  }
}
```

**示例输出：**

如果你的文本文件内容为：

```
Hello, Dart!
```

以上两种方法都会输出：

```
Hello, Dart!
```

**使用第三方库：**

为了获得更多功能，如简化的文件操作或增强的错误处理，你可能会考虑使用第三方库，例如 `package:file`。然而，根据我最后的更新，如上所示直接使用核心 `dart:io` 包是读取 Dart 中文本文件最常见和直接的方法。
