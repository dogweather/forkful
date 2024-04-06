---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:39.422397-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A Dart \u7684\u6838\u5FC3\u5E93 `dart:io`\
  \ \u63D0\u4F9B\u4E86\u540C\u6B65\u6216\u5F02\u6B65\u8BFB\u53D6\u6587\u672C\u6587\
  \u4EF6\u6240\u9700\u7684\u529F\u80FD\u3002\u4EE5\u4E0B\u662F\u4E24\u79CD\u65B9\u6CD5\
  \u7684\u64CD\u4F5C\u6307\u5357\u3002 **\u540C\u6B65\u65B9\u5F0F\uFF1A**."
lastmod: '2024-04-05T22:38:46.602056-06:00'
model: gpt-4-0125-preview
summary: "io` \u63D0\u4F9B\u4E86\u540C\u6B65\u6216\u5F02\u6B65\u8BFB\u53D6\u6587\u672C\
  \u6587\u4EF6\u6240\u9700\u7684\u529F\u80FD\u3002\u4EE5\u4E0B\u662F\u4E24\u79CD\u65B9\
  \u6CD5\u7684\u64CD\u4F5C\u6307\u5357\u3002 **\u540C\u6B65\u65B9\u5F0F\uFF1A**."
title: "\u8BFB\u53D6\u6587\u672C\u6587\u4EF6"
weight: 22
---

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
