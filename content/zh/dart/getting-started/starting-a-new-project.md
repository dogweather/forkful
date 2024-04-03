---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:55.464742-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A 1. **\u5B89\u88C5 Dart**\uFF1A \u786E\
  \u4FDD\u7CFB\u7EDF\u4E0A\u5DF2\u5B89\u88C5 Dart\u3002\u5982\u679C\u6CA1\u6709\uFF0C\
  \u60A8\u53EF\u4EE5\u4ECE [https://dart.dev/get-dart](https://dart.dev/get-dart)\
  \ \u4E0B\u8F7D\u3002\u7528\u4E0B\u9762\u7684\u547D\u4EE4\u9A8C\u8BC1\u5B89\u88C5\
  \uFF1A."
lastmod: '2024-03-13T22:44:47.418842-06:00'
model: gpt-4-0125-preview
summary: '1.'
title: "\u542F\u52A8\u65B0\u9879\u76EE"
weight: 1
---

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
