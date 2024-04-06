---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:50.165398-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A Dart \u5E76\u6CA1\u6709\u5185\u5EFA\u7684\
  \ REPL\u3002\u7136\u800C\uFF0C\u4F60\u53EF\u4EE5\u901A\u8FC7\u4F7F\u7528 DartPad\uFF08\
  \u5728\u7EBF\uFF09\u6216\u5229\u7528\u7B2C\u4E09\u65B9\u5DE5\u5177\u5982 `dart_repl`\
  \ \u6765\u5B9E\u73B0\u7C7B\u4F3C REPL \u7684\u529F\u80FD\u3002 **\u4F7F\u7528 DartPad\uFF1A\
  ** DartPad (https://dartpad.dev) \u662F\u4E00\u4E2A\u5728\u7EBF Dart\u2026"
lastmod: '2024-04-05T21:53:47.749966-06:00'
model: gpt-4-0125-preview
summary: "**\u4F7F\u7528 DartPad\uFF1A** DartPad (https://dartpad.dev) \u662F\u4E00\
  \u4E2A\u5728\u7EBF Dart \u7F16\u8F91\u5668\uFF0C\u8BA9\u4F60\u53EF\u4EE5\u5728\u7F51\
  \u9875\u6D4F\u89C8\u5668\u4E2D\u7F16\u5199\u5E76\u8FD0\u884C Dart \u4EE3\u7801\u3002\
  \u867D\u7136\u5B83\u4E0D\u662F\u4F20\u7EDF\u7684\u547D\u4EE4\u884C REPL\uFF0C\u4F46\
  \u5B83\u4E3A\u5FEB\u901F\u8BD5\u9A8C\u63D0\u4F9B\u4E86\u7C7B\u4F3C\u7684\u4F53\u9A8C\
  ."
title: "\u4F7F\u7528\u4EA4\u4E92\u5F0FShell\uFF08REPL\uFF09"
weight: 34
---

## 如何操作：
Dart 并没有内建的 REPL。然而，你可以通过使用 DartPad（在线）或利用第三方工具如 `dart_repl` 来实现类似 REPL 的功能。

**使用 DartPad：**

DartPad (https://dartpad.dev) 是一个在线 Dart 编辑器，让你可以在网页浏览器中编写并运行 Dart 代码。虽然它不是传统的命令行 REPL，但它为快速试验提供了类似的体验。

只需访问该网站，将你的 Dart 代码输入左侧面板，然后点击 “运行” 来查看右侧的输出。

示例：
```dart
void main() {
  print('Hello, Dart!');
}
```
输出：
```
Hello, Dart!
```

**使用 `dart_repl`（第三方工具）：**

首先，通过 pub 全局安装 `dart_repl`：

```shell
dart pub global activate dart_repl
```

然后，从你的终端运行 `dart_repl`：

```shell
dart_repl
```

现在，你可以开始直接在 shell 中输入 Dart 语句。例如：

```dart
>>> print('Hello, REPL!');
Hello, REPL!
>>> int add(int x, int y) => x + y;
>>> print(add(5, 7));
12
```

这些方法为即时尝试 Dart 代码提供了快速途径，显著降低了学习曲线并提高了生产力。
