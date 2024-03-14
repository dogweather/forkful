---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:55.464742-07:00
description: "\u5728 Dart \u4E2D\u542F\u52A8\u4E00\u4E2A\u65B0\u9879\u76EE\u6D89\u53CA\
  \u5230\u8BBE\u7F6E\u4E00\u4E2A\u6709\u52A9\u4E8E\u9AD8\u6548\u5F00\u53D1\u3001\u6D4B\
  \u8BD5\u548C\u90E8\u7F72\u7684\u73AF\u5883\u3002\u7A0B\u5E8F\u5458\u542F\u52A8\u65B0\
  \u7684 Dart \u9879\u76EE\u4EE5\u5229\u7528 Dart \u7684\u6700\u4F73\u6027\u80FD\u548C\
  \u5F3A\u5927\u751F\u6001\u7CFB\u7EDF\uFF0C\u7279\u522B\u662F\u7528\u4E8E\u4F7F\u7528\
  \u50CF Flutter \u8FD9\u6837\u7684\u6846\u67B6\u7684 Web \u548C\u79FB\u52A8\u5E94\
  \u7528\u7A0B\u5E8F\u5F00\u53D1\u3002"
lastmod: '2024-03-13T22:44:47.418842-06:00'
model: gpt-4-0125-preview
summary: "\u5728 Dart \u4E2D\u542F\u52A8\u4E00\u4E2A\u65B0\u9879\u76EE\u6D89\u53CA\
  \u5230\u8BBE\u7F6E\u4E00\u4E2A\u6709\u52A9\u4E8E\u9AD8\u6548\u5F00\u53D1\u3001\u6D4B\
  \u8BD5\u548C\u90E8\u7F72\u7684\u73AF\u5883\u3002\u7A0B\u5E8F\u5458\u542F\u52A8\u65B0\
  \u7684 Dart \u9879\u76EE\u4EE5\u5229\u7528 Dart \u7684\u6700\u4F73\u6027\u80FD\u548C\
  \u5F3A\u5927\u751F\u6001\u7CFB\u7EDF\uFF0C\u7279\u522B\u662F\u7528\u4E8E\u4F7F\u7528\
  \u50CF Flutter \u8FD9\u6837\u7684\u6846\u67B6\u7684 Web \u548C\u79FB\u52A8\u5E94\
  \u7528\u7A0B\u5E8F\u5F00\u53D1\u3002"
title: "\u542F\u52A8\u65B0\u9879\u76EE"
---

{{< edit_this_page >}}

## 什么和为什么？

在 Dart 中启动一个新项目涉及到设置一个有助于高效开发、测试和部署的环境。程序员启动新的 Dart 项目以利用 Dart 的最佳性能和强大生态系统，特别是用于使用像 Flutter 这样的框架的 Web 和移动应用程序开发。

## 如何操作：

1. **安装 Dart**：
   确保系统上已安装 Dart。如果没有，您可以从 [https://dart.dev/get-dart](https://dart.dev/get-dart) 下载。用下面的命令验证安装：

   ```shell
   dart --version
   ```

2. **创建一个新的 Dart 项目**：
   使用 Dart CLI 生成一个新项目：

   ```shell
   dart create hello_dart
   ```

   该命令创建一个新目录 `hello_dart`，内含一个简单的示例 Web 或控制台应用程序，这取决于您的选择。

3. **查看项目结构**：

   导航到您的项目目录：

   ```shell
   cd hello_dart
   ```

   一个典型的 Dart 项目包括以下关键文件和目录：

   - `pubspec.yaml`：包含项目依赖项和 SDK 约束的配置文件。
   - `lib/`：大部分 Dart 代码存在的目录。
   - `test/`：项目测试的目录。

4. **添加依赖项**：
   编辑 `pubspec.yaml` 来添加依赖项。对于 web 项目，考虑添加 `http`，这是一个用于发起 HTTP 请求的流行包：

   ```yaml
   dependencies:
     flutter:
       sdk: flutter
     http: ^0.13.3
   ```

   编辑后，获取依赖项：

   ```shell
   dart pub get
   ```

5. **编写您的第一个 Dart 代码**：

   在 `lib/` 目录中，创建一个新的 Dart 文件，`main.dart`，并添加一段简单的 Dart 代码：

   ```dart
   // 导入 Dart 核心库
   import 'dart:core';

   void main() {
     print('Hello, Dart!');
   }
   ```

6. **运行您的 Dart 应用程序**：

   使用以下命令执行您的 Dart 程序：

   ```shell
   dart run
   ```

   输出应该是：

   ```
   Hello, Dart!
   ```

按照这些步骤，您已成功开始一个新的 Dart 项目，从安装到运行您的第一个 Dart 代码片段。这些基础知识为深入了解 Dart 的丰富生态系统及其构建可扩展应用程序的能力奠定了基础。
