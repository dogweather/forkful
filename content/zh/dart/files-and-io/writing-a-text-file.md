---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:57:46.094136-07:00
description: "\u5728 Dart \u4E2D\u5199\u5165\u6587\u672C\u6587\u4EF6\u6D89\u53CA\u5728\
  \u78C1\u76D8\u4E0A\u521B\u5EFA\u6216\u4FEE\u6539\u6587\u4EF6\uFF0C\u4EE5\u4FBF\u4EE5\
  \u53EF\u8BFB\u683C\u5F0F\u5B58\u50A8\u6570\u636E\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\
  \u505A\u662F\u4E3A\u4E86\u4FDD\u5B58\u5E94\u7528\u7A0B\u5E8F\u6570\u636E\u3001\u914D\
  \u7F6E\u3001\u65E5\u5FD7\u6216\u4EFB\u4F55\u5E94\u8BE5\u5728\u5E94\u7528\u7A0B\u5E8F\
  \u8FD0\u884C\u4E4B\u95F4\u4FDD\u6301\u6216\u4E0E\u5176\u4ED6\u5E94\u7528\u7A0B\u5E8F\
  \u6216\u7528\u6237\u5171\u4EAB\u7684\u4FE1\u606F\u3002"
lastmod: '2024-03-11T00:14:21.203340-06:00'
model: gpt-4-0125-preview
summary: "\u5728 Dart \u4E2D\u5199\u5165\u6587\u672C\u6587\u4EF6\u6D89\u53CA\u5728\
  \u78C1\u76D8\u4E0A\u521B\u5EFA\u6216\u4FEE\u6539\u6587\u4EF6\uFF0C\u4EE5\u4FBF\u4EE5\
  \u53EF\u8BFB\u683C\u5F0F\u5B58\u50A8\u6570\u636E\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\
  \u505A\u662F\u4E3A\u4E86\u4FDD\u5B58\u5E94\u7528\u7A0B\u5E8F\u6570\u636E\u3001\u914D\
  \u7F6E\u3001\u65E5\u5FD7\u6216\u4EFB\u4F55\u5E94\u8BE5\u5728\u5E94\u7528\u7A0B\u5E8F\
  \u8FD0\u884C\u4E4B\u95F4\u4FDD\u6301\u6216\u4E0E\u5176\u4ED6\u5E94\u7528\u7A0B\u5E8F\
  \u6216\u7528\u6237\u5171\u4EAB\u7684\u4FE1\u606F\u3002"
title: "\u7F16\u5199\u6587\u672C\u6587\u4EF6"
---

{{< edit_this_page >}}

## 什么 & 为什么？
在 Dart 中写入文本文件涉及在磁盘上创建或修改文件，以便以可读格式存储数据。程序员这样做是为了保存应用程序数据、配置、日志或任何应该在应用程序运行之间保持或与其他应用程序或用户共享的信息。

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
