---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:42.782448-07:00
description: "\u5728 Dart \u4E2D\u4F7F\u7528\u8C03\u8BD5\u5668\u5141\u8BB8\u7A0B\u5E8F\
  \u5458\u901A\u8FC7\u8BBE\u7F6E\u65AD\u70B9\u3001\u9010\u6B65\u6267\u884C\u548C\u68C0\
  \u67E5\u53D8\u91CF\uFF0C\u6709\u6761\u7406\u5730\u68C0\u67E5\u4ED6\u4EEC\u7684\u4EE3\
  \u7801\u3002\u8FD9\u4E2A\u8FC7\u7A0B\u5BF9\u4E8E\u9AD8\u6548\u5730\u8BC6\u522B\u548C\
  \u4FEE\u590D\u9519\u8BEF\u81F3\u5173\u91CD\u8981\uFF0C\u56E0\u6B64\u5B83\u662F\u5F00\
  \u53D1\u751F\u547D\u5468\u671F\u4E2D\u4E0D\u53EF\u6216\u7F3A\u7684\u5DE5\u5177\u3002"
lastmod: '2024-03-13T22:44:47.424315-06:00'
model: gpt-4-0125-preview
summary: "\u5728 Dart \u4E2D\u4F7F\u7528\u8C03\u8BD5\u5668\u5141\u8BB8\u7A0B\u5E8F\
  \u5458\u901A\u8FC7\u8BBE\u7F6E\u65AD\u70B9\u3001\u9010\u6B65\u6267\u884C\u548C\u68C0\
  \u67E5\u53D8\u91CF\uFF0C\u6709\u6761\u7406\u5730\u68C0\u67E5\u4ED6\u4EEC\u7684\u4EE3\
  \u7801\u3002\u8FD9\u4E2A\u8FC7\u7A0B\u5BF9\u4E8E\u9AD8\u6548\u5730\u8BC6\u522B\u548C\
  \u4FEE\u590D\u9519\u8BEF\u81F3\u5173\u91CD\u8981\uFF0C\u56E0\u6B64\u5B83\u662F\u5F00\
  \u53D1\u751F\u547D\u5468\u671F\u4E2D\u4E0D\u53EF\u6216\u7F3A\u7684\u5DE5\u5177\u3002"
title: "\u4F7F\u7528\u8C03\u8BD5\u5668"
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
