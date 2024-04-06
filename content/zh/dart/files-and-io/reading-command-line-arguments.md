---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:44.396993-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A Dart \u901A\u8FC7\u5728 main \u65B9\u6CD5\
  \u4E2D\u7684 `List<String> args` \u63D0\u4F9B\u4E86\u4E00\u79CD\u76F4\u63A5\u8BBF\
  \u95EE\u547D\u4EE4\u884C\u53C2\u6570\u7684\u7B80\u5355\u65B9\u6CD5\u3002\u4EE5\u4E0B\
  \u662F\u4E00\u4E2A\u7B80\u5355\u7684\u793A\u4F8B\uFF0C\u5C55\u793A\u5982\u4F55\u8BFB\
  \u53D6\u548C\u4F7F\u7528\u547D\u4EE4\u884C\u53C2\u6570\u3002"
lastmod: '2024-04-05T22:38:46.599915-06:00'
model: gpt-4-0125-preview
summary: "\u5982\u4F55\u64CD\u4F5C\uFF1A Dart \u901A\u8FC7\u5728 main \u65B9\u6CD5\
  \u4E2D\u7684 `List<String> args` \u63D0\u4F9B\u4E86\u4E00\u79CD\u76F4\u63A5\u8BBF\
  \u95EE\u547D\u4EE4\u884C\u53C2\u6570\u7684\u7B80\u5355\u65B9\u6CD5\u3002\u4EE5\u4E0B\
  \u662F\u4E00\u4E2A\u7B80\u5355\u7684\u793A\u4F8B\uFF0C\u5C55\u793A\u5982\u4F55\u8BFB\
  \u53D6\u548C\u4F7F\u7528\u547D\u4EE4\u884C\u53C2\u6570\u3002"
title: "\u8BFB\u53D6\u547D\u4EE4\u884C\u53C2\u6570"
weight: 23
---

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
