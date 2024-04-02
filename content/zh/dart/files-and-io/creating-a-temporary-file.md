---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:54:07.336253-07:00
description: "\u5728 Dart \u4E2D\u521B\u5EFA\u4E00\u4E2A\u4E34\u65F6\u6587\u4EF6\u6D89\
  \u53CA\u751F\u6210\u4E00\u4E2A\u610F\u56FE\u7528\u4E8E\u77ED\u671F\u7684\u6587\u4EF6\
  \uFF0C\u4E3B\u8981\u7528\u4E8E\u573A\u666F\u5982\u6570\u636E\u7F13\u5B58\u3001\u6587\
  \u4EF6\u5904\u7406\u7684\u4E34\u65F6\u5B58\u50A8\uFF0C\u6216\u4FDD\u7559\u4FE1\u606F\
  \u592A\u654F\u611F\u800C\u4E0D\u80FD\u957F\u671F\u4FDD\u5B58\u3002\u7A0B\u5E8F\u5458\
  \u8FD9\u6837\u505A\u662F\u4E3A\u4E86\u7BA1\u7406\u90A3\u4E9B\u4E0D\u9700\u8981\u6C38\
  \u4E45\u5B58\u50A8\u7684\u6570\u636E\uFF0C\u4ECE\u800C\u63D0\u5347\u6027\u80FD\u5E76\
  \u4FDD\u6301\u6570\u636E\u536B\u751F\u3002"
lastmod: '2024-03-13T22:44:47.444447-06:00'
model: gpt-4-0125-preview
summary: "\u5728 Dart \u4E2D\u521B\u5EFA\u4E00\u4E2A\u4E34\u65F6\u6587\u4EF6\u6D89\
  \u53CA\u751F\u6210\u4E00\u4E2A\u610F\u56FE\u7528\u4E8E\u77ED\u671F\u7684\u6587\u4EF6\
  \uFF0C\u4E3B\u8981\u7528\u4E8E\u573A\u666F\u5982\u6570\u636E\u7F13\u5B58\u3001\u6587\
  \u4EF6\u5904\u7406\u7684\u4E34\u65F6\u5B58\u50A8\uFF0C\u6216\u4FDD\u7559\u4FE1\u606F\
  \u592A\u654F\u611F\u800C\u4E0D\u80FD\u957F\u671F\u4FDD\u5B58\u3002\u7A0B\u5E8F\u5458\
  \u8FD9\u6837\u505A\u662F\u4E3A\u4E86\u7BA1\u7406\u90A3\u4E9B\u4E0D\u9700\u8981\u6C38\
  \u4E45\u5B58\u50A8\u7684\u6570\u636E\uFF0C\u4ECE\u800C\u63D0\u5347\u6027\u80FD\u5E76\
  \u4FDD\u6301\u6570\u636E\u536B\u751F\u3002"
title: "\u521B\u5EFA\u4E34\u65F6\u6587\u4EF6"
weight: 21
---

## 什么和为什么？
在 Dart 中创建一个临时文件涉及生成一个意图用于短期的文件，主要用于场景如数据缓存、文件处理的临时存储，或保留信息太敏感而不能长期保存。程序员这样做是为了管理那些不需要永久存储的数据，从而提升性能并保持数据卫生。

## 如何操作：
Dart 的 `dart:io` 库通过 `Directory` 类促进了临时文件的创建。这里是创建一个临时文件并向其写入一些内容的直接方法：

```dart
import 'dart:io';

Future<void> main() async {
  // 创建一个临时目录（系统特定位置）
  Directory tempDir = await Directory.systemTemp.createTemp('my_temp_dir_');

  // 在该目录内创建一个临时文件
  File tempFile = File('${tempDir.path}/my_temp_file.txt');

  // 向临时文件写入一些内容
  await tempFile.writeAsString('This is some temporary content');

  print('临时文件已创建: ${tempFile.path}');

  // 示例输出: 临时文件已创建: /tmp/my_temp_dir_A1B2C3/my_temp_file.txt
}
```

### 使用第三方库：`path_provider`

对于应用程序（尤其是带有 Flutter 的移动应用），您可能希望建立一种更统一和可管理的方式来创建临时文件。`path_provider` 包可以帮助您在不同平台（iOS、安卓等）上找到正确的临时目录。

首先，在您的 `pubspec.yaml` 下的依赖项中添加 `path_provider`：

```yaml
dependencies:
  path_provider: ^2.0.9
```

这里是如何使用它来创建一个临时文件：

```dart
import 'dart:io';
import 'package:path_provider/path_provider.dart';

Future<void> main() async {
  // 获取临时目录
  final Directory tempDir = await getTemporaryDirectory();

  // 在该目录内创建一个临时文件
  final File tempFile = File('${tempDir.path}/my_temp_file.txt');

  // 向临时文件写入一些内容
  await tempFile.writeAsString('This is some temporary content with path_provider');

  print('临时文件已用 path_provider 创建: ${tempFile.path}');

  // 示例输出: 临时文件已用 path_provider 创建: /tmp/my_temp_file.txt (路径可能因平台而异)
}
```

这些片段展示了在 Dart 中创建和交互临时文件的方法，为短期目的的数据管理提供了一种直接且实用的方式。
