---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:57:46.094136-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A Dart \u7684\u6838\u5FC3\u5E93\u63D0\u4F9B\
  \u4E86 `dart:io` \u5305\u7528\u4E8E\u6587\u4EF6\u5904\u7406\uFF0C\u4F7F\u60A8\u53EF\
  \u4EE5\u65E0\u9700\u7B2C\u4E09\u65B9\u5E93\u5373\u53EF\u5199\u5165\u6587\u672C\u6587\
  \u4EF6\u3002\u4E0B\u9762\u662F\u4E00\u4E2A\u5199\u5165\u6587\u672C\u6587\u4EF6\u7684\
  \u7B80\u5355\u793A\u4F8B\uFF1A."
lastmod: '2024-04-05T22:38:46.603131-06:00'
model: gpt-4-0125-preview
summary: "io` \u5305\u7528\u4E8E\u6587\u4EF6\u5904\u7406\uFF0C\u4F7F\u60A8\u53EF\u4EE5\
  \u65E0\u9700\u7B2C\u4E09\u65B9\u5E93\u5373\u53EF\u5199\u5165\u6587\u672C\u6587\u4EF6\
  \u3002\u4E0B\u9762\u662F\u4E00\u4E2A\u5199\u5165\u6587\u672C\u6587\u4EF6\u7684\u7B80\
  \u5355\u793A\u4F8B\uFF1A."
title: "\u7F16\u5199\u6587\u672C\u6587\u4EF6"
weight: 24
---

## 如何操作：
Dart 的核心库提供了 `dart:io` 包用于文件处理，使您可以无需第三方库即可写入文本文件。下面是一个写入文本文件的简单示例：

```dart
import 'dart:io';

void main() async {
  // 在当前目录下创建一个名为 'example.txt' 的新文件。
  var file = File('example.txt');
  
  // 将字符串写入文件。
  await file.writeAsString('Hello, Dart!');
  
  // 验证内容。
  print(await file.readAsString()); // 输出：Hello, Dart!
}
```

处理更大的文件或数据流时，您可能更喜欢使用 `openWrite` 来写入内容，它返回一个 `IOSink` 并允许您分块写入数据：

```dart
import 'dart:io';

void main() async {
  var file = File('large_file.txt');
  var sink = file.openWrite();

  // 向文件写入多行内容。
  sink
    ..writeln('Line 1: The quick brown fox jumps over the lazy dog.')
    ..writeln('Line 2: Dart is awesome!')
    ..close();

  // 等待 sink 关闭以确保所有数据都写入到文件中。
  await sink.done;

  // 读取并打印文件内容以进行验证
  print(await file.readAsString());
}
```

对于更高级的文件操作，包括向文件追加或写入字节，您可以深入了解 `dart:io` 提供的 `File` 类方法。此外，在处理大规模或更复杂的项目时，考虑使用像 `path` 这样的包处理文件路径或 `shelf` 提供网站服务器功能可能是有益的，尽管直接写入文件通常依赖于内置的 Dart 库。
