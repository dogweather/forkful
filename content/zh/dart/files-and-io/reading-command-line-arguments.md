---
title:                "读取命令行参数"
date:                  2024-03-08T21:55:44.396993-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

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
