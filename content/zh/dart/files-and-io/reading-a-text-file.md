---
title:                "读取文本文件"
date:                  2024-03-08T21:55:39.422397-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

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
