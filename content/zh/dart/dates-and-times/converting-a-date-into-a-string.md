---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:53:44.974512-07:00
description: "\u5728Dart\u4E2D\uFF0C\u5C06\u65E5\u671F\u8F6C\u6362\u4E3A\u5B57\u7B26\
  \u4E32\u662F\u4E00\u4E2A\u5E38\u89C1\u4EFB\u52A1\uFF0C\u5F53\u60A8\u9700\u8981\u4EE5\
  \u6613\u4E8E\u9605\u8BFB\u7684\u683C\u5F0F\u663E\u793A\u65E5\u671F\u548C\u65F6\u95F4\
  \u4FE1\u606F\uFF0C\u6216\u8005\u6253\u7B97\u5E8F\u5217\u5316\u6570\u636E\u4EE5\u4FBF\
  \u5B58\u50A8\u6216\u4F20\u8F93\u65F6\uFF0C\u6B64\u64CD\u4F5C\u5C31\u663E\u5F97\u5C24\
  \u4E3A\u91CD\u8981\u3002\u8FD9\u4E2A\u8FC7\u7A0B\u5141\u8BB8\u4EE5\u4E00\u79CD\u6613\
  \u4E8E\u7406\u89E3\u4E14\u53EF\u6839\u636E\u7528\u4F8B\u5B9A\u5236\u7684\u683C\u5F0F\
  \uFF0C\u8F7B\u677E\u8868\u793A\u548C\u64CD\u4F5C\u65E5\u671F\u65F6\u95F4\u503C\u3002"
lastmod: '2024-03-13T22:44:47.433908-06:00'
model: gpt-4-0125-preview
summary: "\u5728Dart\u4E2D\uFF0C\u5C06\u65E5\u671F\u8F6C\u6362\u4E3A\u5B57\u7B26\u4E32\
  \u662F\u4E00\u4E2A\u5E38\u89C1\u4EFB\u52A1\uFF0C\u5F53\u60A8\u9700\u8981\u4EE5\u6613\
  \u4E8E\u9605\u8BFB\u7684\u683C\u5F0F\u663E\u793A\u65E5\u671F\u548C\u65F6\u95F4\u4FE1\
  \u606F\uFF0C\u6216\u8005\u6253\u7B97\u5E8F\u5217\u5316\u6570\u636E\u4EE5\u4FBF\u5B58\
  \u50A8\u6216\u4F20\u8F93\u65F6\uFF0C\u6B64\u64CD\u4F5C\u5C31\u663E\u5F97\u5C24\u4E3A\
  \u91CD\u8981\u3002\u8FD9\u4E2A\u8FC7\u7A0B\u5141\u8BB8\u4EE5\u4E00\u79CD\u6613\u4E8E\
  \u7406\u89E3\u4E14\u53EF\u6839\u636E\u7528\u4F8B\u5B9A\u5236\u7684\u683C\u5F0F\uFF0C\
  \u8F7B\u677E\u8868\u793A\u548C\u64CD\u4F5C\u65E5\u671F\u65F6\u95F4\u503C\u3002."
title: "\u5C06\u65E5\u671F\u8F6C\u6362\u4E3A\u5B57\u7B26\u4E32"
weight: 28
---

## 什么及为什么？

在Dart中，将日期转换为字符串是一个常见任务，当您需要以易于阅读的格式显示日期和时间信息，或者打算序列化数据以便存储或传输时，此操作就显得尤为重要。这个过程允许以一种易于理解且可根据用例定制的格式，轻松表示和操作日期时间值。

## 如何操作：

Dart提供了用于处理日期和时间的`DateTime`类，以及用于格式化的`intl`包。首先，确保通过在`pubspec.yaml`文件中添加`intl: ^0.17.0`（或最新版本）来包含`intl`包。

### 使用Dart的核心库

```dart
DateTime now = DateTime.now();
String formattedDate = "${now.year}-${now.month}-${now.day}";
print(formattedDate); // 输出：2023-4-12（例如，这取决于当前日期）
```

此示例直接从`DateTime`的属性构造字符串。

### 使用`intl`包

首先，导入包：

```dart
import 'package:intl/intl.dart';
```

然后，格式化日期：

```dart
DateTime now = DateTime.now();
String formattedDate = DateFormat('yyyy-MM-dd').format(now);
print(formattedDate); // 输出：2023-04-12
```

`intl`包允许轻松地进行更复杂的格式化，包括特定于区域设置的格式：

```dart
String formattedDateLocale = DateFormat.yMMMMd('en_US').format(now);
print(formattedDateLocale); // 输出：April 12, 2023
```

这些示例展示了在Dart中将日期转换并格式化为字符串的简单但强大的方法，无论是使用Dart的核心功能还是利用`intl`包进行更高级的格式化选项。
