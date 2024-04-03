---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:58:03.045145-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u5728 Dart \u4E2D\uFF0C\u4F7F\u7528\
  \ `dart:io` \u4E2D\u53EF\u7528\u7684 `stderr` \u5BF9\u8C61\u6765\u5199\u5165 stderr\
  \ \u662F\u5F88\u76F4\u63A5\u7684\u3002\u8FD9\u662F\u4E00\u4E2A\u57FA\u672C\u793A\
  \u4F8B\uFF1A."
lastmod: '2024-03-13T22:44:47.440514-06:00'
model: gpt-4-0125-preview
summary: "\u5728 Dart \u4E2D\uFF0C\u4F7F\u7528 `dart:io` \u4E2D\u53EF\u7528\u7684\
  \ `stderr` \u5BF9\u8C61\u6765\u5199\u5165 stderr \u662F\u5F88\u76F4\u63A5\u7684\u3002\
  \u8FD9\u662F\u4E00\u4E2A\u57FA\u672C\u793A\u4F8B\uFF1A."
title: "\u5199\u5165\u6807\u51C6\u9519\u8BEF"
weight: 25
---

## 如何操作：
在 Dart 中，使用 `dart:io` 中可用的 `stderr` 对象来写入 stderr 是很直接的。这是一个基本示例：

```dart
import 'dart:io';

void main() {
  stderr.writeln('这是一个错误消息。');
}
```

运行时的输出：
```
这是一个错误消息。
```
此消息被发送到 stderr 流，通常在控制台或终端中显示。

为了演示更复杂的情况，例如记录一个异常，Dart 的丰富特性集允许进行简洁有效的错误处理：

```dart
import 'dart:io';

void riskyOperation() {
  try {
    // 模拟可能抛出异常的操作
    throw Exception('出了点问题！');
  } catch (e) {
    stderr.writeln('错误：$e');
  }
}

void main() {
  riskyOperation();
}
```

运行时的输出：
```
错误：异常：出了点问题！
```

这种模式对于需要将正常日志与错误日志分开的应用程序特别有用，使得监控和调试应用程序更加容易。

虽然 Dart 的标准库相当全面，但许多程序并不需要第三方库就可以写入 stderr。然而，如果你的应用程序需要更复杂的日志功能（例如，写入文件、通过网络、格式化），那么 `logging` 包是一个流行的选择。这里快速看一下使用 `logging` 记录错误的情况：

```dart
import 'dart:io';
import 'package:logging/logging.dart';

final logger = Logger('MyAppLogger');

void setupLogging() {
  logger.onRecord.listen((record) {
    if (record.level >= Level.SEVERE) {
      stderr.writeln('${record.level.name}: ${record.time}: ${record.message}');
    }
  });
}

void main() {
  setupLogging();
  logger.severe('严重错误：发生了非常糟糕的事情。');
}
```

运行时的输出：
```
SEVERE: 2023-04-01 00:00:00.000: 严重错误：发生了非常糟糕的事情。
```

这种方法提供了更高程度的定制化和控制，你可以决定什么被记录为错误以及如何格式化，这在更大、更复杂的应用程序中非常有帮助。
