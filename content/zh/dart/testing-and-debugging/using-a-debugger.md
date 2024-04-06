---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:42.782448-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A **1. \u8BBE\u7F6E\u65AD\u70B9\uFF1A**\
  \ \u8981\u8BBE\u7F6E\u4E00\u4E2A\u65AD\u70B9\uFF0C\u53EA\u9700\u5728\u60A8\u5E0C\
  \u671B\u6267\u884C\u6682\u505C\u7684\u4EE3\u7801\u884C\u7684\u5DE6\u8FB9\u8DDD\u4E2D\
  \u5355\u51FB\uFF08\u4F8B\u5982\uFF0C\u5728 Visual Studio Code \u6216 Android Studio\
  \ \u4E2D\uFF09\u3002"
lastmod: '2024-04-05T22:38:46.586850-06:00'
model: gpt-4-0125-preview
summary: "\u5982\u4F55\u64CD\u4F5C\uFF1A **1. \u8BBE\u7F6E\u65AD\u70B9\uFF1A** \u8981\
  \u8BBE\u7F6E\u4E00\u4E2A\u65AD\u70B9\uFF0C\u53EA\u9700\u5728\u60A8\u5E0C\u671B\u6267\
  \u884C\u6682\u505C\u7684\u4EE3\u7801\u884C\u7684\u5DE6\u8FB9\u8DDD\u4E2D\u5355\u51FB\
  \uFF08\u4F8B\u5982\uFF0C\u5728 Visual Studio Code \u6216 Android Studio \u4E2D\uFF09\
  \u3002"
title: "\u4F7F\u7528\u8C03\u8BD5\u5668"
weight: 35
---

## 如何操作：


### 基础调试：
**1. 设置断点：**

要设置一个断点，只需在您希望执行暂停的代码行的左边距中单击（例如，在 Visual Studio Code 或 Android Studio 中）。

```dart
void main() {
  var message = 'Hello, Debugging';
  print(message); // 在这里设置一个断点
}
```

**2. 开始调试：**

在您的 IDE 中，通过点击调试图标或按下调试按钮来启动一个调试会话。执行将在断点处暂停。

**3. 检查变量：**

一旦执行暂停，将鼠标悬停在变量上查看其当前值。

**4. 逐步执行代码：**

使用您的 IDE 中的跳过（step over）、跳入（step into）和跳出（step out）命令，逐行或逐函数地导航通过您的代码。

### 使用 Observatory 进行高级调试：
Dart 包括一个叫做 Observatory 的工具，用于调试和分析 Dart 应用程序。它对于运行在 Dart VM 上的应用程序特别有用。

**访问 Observatory：**

用 `--observe` 标志运行您的 Dart 应用程序。

```bash
dart --observe your_program.dart
```

此命令会将一个 URL 打印到控制台，您可以在 Web 浏览器中打开此 URL 以访问 Observatory 调试器。

### 使用流行的第三方库：
对于调试 Flutter 应用程序，`flutter_devtools` 包提供了一套性能和调试工具，与 Dart VM 和 Flutter 都集成。

**安装：**

首先，在您的 `pubspec.yaml` 文件中的 `dev_dependencies` 下添加 `devtools`：

```yaml
dev_dependencies:
  devtools: any
```

**启动 DevTools：**

在终端运行此命令：

```bash
flutter pub global run devtools
```

然后，以调试模式启动您的 Flutter 应用程序。DevTools 提供的功能，如 Flutter 检查器用于部件树分析，和网络分析器用于监控网络活动。

### 样本输出：
当触发一个断点时，您的 IDE 可能会像这样显示变量值和堆栈跟踪：

```
message: 'Hello, Debugging'
```

通过有效地利用 Dart 中的调试工具和技术，开发者可以更快地识别和解决问题，从而使得开发过程更加顺畅，并创建出更健壮的应用程序。
