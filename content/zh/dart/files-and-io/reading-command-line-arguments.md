---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:44.396993-07:00
description: "\u5728 Dart \u4E2D\u8BFB\u53D6\u547D\u4EE4\u884C\u53C2\u6570\u4F7F\u7A0B\
  \u5E8F\u5458\u80FD\u591F\u5728\u6267\u884C Dart \u7A0B\u5E8F\u65F6\u76F4\u63A5\u5411\
  \u63A7\u5236\u53F0\u8F93\u5165\u6570\u636E\uFF0C\u63D0\u9AD8\u4E86\u5176\u4EA4\u4E92\
  \u6027\u548C\u9488\u5BF9\u5404\u79CD\u7528\u4F8B\uFF08\u5305\u62EC\u81EA\u52A8\u5316\
  \u811A\u672C\u3001CLI \u5DE5\u5177\u6216\u6279\u5904\u7406\uFF09\u7684\u7075\u6D3B\
  \u6027\u3002\u8FD9\u4E00\u529F\u80FD\u5BF9\u4E8E\u521B\u5EFA\u9002\u5E94\u6027\u5F3A\
  \u548C\u7528\u6237\u53CB\u597D\u7684\u547D\u4EE4\u884C\u5E94\u7528\u7A0B\u5E8F\u81F3\
  \u5173\u91CD\u8981\u3002"
lastmod: '2024-03-13T22:44:47.439239-06:00'
model: gpt-4-0125-preview
summary: "\u5728 Dart \u4E2D\u8BFB\u53D6\u547D\u4EE4\u884C\u53C2\u6570\u4F7F\u7A0B\
  \u5E8F\u5458\u80FD\u591F\u5728\u6267\u884C Dart \u7A0B\u5E8F\u65F6\u76F4\u63A5\u5411\
  \u63A7\u5236\u53F0\u8F93\u5165\u6570\u636E\uFF0C\u63D0\u9AD8\u4E86\u5176\u4EA4\u4E92\
  \u6027\u548C\u9488\u5BF9\u5404\u79CD\u7528\u4F8B\uFF08\u5305\u62EC\u81EA\u52A8\u5316\
  \u811A\u672C\u3001CLI \u5DE5\u5177\u6216\u6279\u5904\u7406\uFF09\u7684\u7075\u6D3B\
  \u6027\u3002\u8FD9\u4E00\u529F\u80FD\u5BF9\u4E8E\u521B\u5EFA\u9002\u5E94\u6027\u5F3A\
  \u548C\u7528\u6237\u53CB\u597D\u7684\u547D\u4EE4\u884C\u5E94\u7528\u7A0B\u5E8F\u81F3\
  \u5173\u91CD\u8981\u3002."
title: "\u8BFB\u53D6\u547D\u4EE4\u884C\u53C2\u6570"
weight: 23
---

## 什么与为什么？

在 Dart 中读取命令行参数使程序员能够在执行 Dart 程序时直接向控制台输入数据，提高了其交互性和针对各种用例（包括自动化脚本、CLI 工具或批处理）的灵活性。这一功能对于创建适应性强和用户友好的命令行应用程序至关重要。

## 如何操作：

Dart 通过在 main 方法中的 `List<String> args` 提供了一种直接访问命令行参数的简单方法。以下是一个简单的示例，展示如何读取和使用命令行参数。

```dart
// main.dart
void main(List<String> args) {
  print('命令行参数：');
  for (var i = 0; i < args.length; i++) {
    print('${i + 1}: ${args[i]}');
  }
}
```

要运行此 Dart 程序并传递命令行参数，请像这样使用 Dart CLI：

```shell
dart run main.dart 你好 世界！
```

预期输出：

```
命令行参数：
1: 你好
2: 世界！
```

### 使用流行的第三方库：`args`

虽然 Dart 内置的命令行参数处理能力对许多应用程序而言已经足够强大，但 `args` 包为更复杂的需求提供了一种定义和解析命令行参数的精细方式。

首先，将 `args` 包添加到您的 `pubspec.yaml` 中：

```yaml
dependencies:
  args: ^2.0.0
```

然后，按以下方式在您的程序中使用它：

```dart
// 使用 'args' 包
import 'package:args/args.dart';

void main(List<String> arguments) {
  final parser = ArgParser()..addOption('name', abbr: 'n');
  final argResults = parser.parse(arguments);

  if (argResults.wasParsed('name')) {
    print('你好，${argResults['name']}！');
  } else {
    print('没有提供姓名。');
  }
}
```

使用命名参数运行程序：

```shell
dart run main.dart --name=John
```

预期输出：

```
你好，John！
```

这个简单的介绍，展示了如何在 Dart 中本地及使用 `args` 库解析命令行参数，展现了 Dart 如何从控制台处理用户输入，为创建更多交互性和动态 CLI 应用程序打开了一条路径。
