---
title:                "使用交互式Shell（REPL）"
date:                  2024-03-08T21:56:50.165398-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## 什么 & 为什么？

为 Dart 设计的交互式 shell（REPL - 读取-求值-打印 循环）允许程序员逐行动态输入并执行 Dart 代码，而无需编译整个脚本。这个工具对于学习 Dart 的语法、试验代码片段或通过提供即时反馈和促进迭代测试来调试来说，都是极其宝贵的。

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
