---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:53:54.965657-07:00
description: "\u5C06\u5B57\u7B26\u4E32\u8F6C\u6362\u4E3A\u5C0F\u5199\u662F\u4E00\u79CD\
  \u57FA\u672C\u64CD\u4F5C\uFF0C\u6D89\u53CA\u5C06\u7ED9\u5B9A\u5B57\u7B26\u4E32\u4E2D\
  \u7684\u6240\u6709\u5B57\u7B26\u8F6C\u6362\u4E3A\u5176\u5C0F\u5199\u7B49\u4EF7\u7269\
  \u3002\u7A0B\u5E8F\u5458\u901A\u5E38\u6267\u884C\u6B64\u64CD\u4F5C\u4EE5\u5B9E\u73B0\
  \u5927\u5C0F\u5199\u4E0D\u654F\u611F\u7684\u6BD4\u8F83\u6216\u6807\u51C6\u5316\u6587\
  \u672C\u8F93\u5165\u4EE5\u4FBF\u8FDB\u4E00\u6B65\u5904\u7406\uFF0C\u4ECE\u800C\u4F7F\
  \u5E94\u7528\u7A0B\u5E8F\u66F4\u52A0\u7528\u6237\u53CB\u597D\uFF0C\u6570\u636E\u66F4\
  \u52A0\u4E00\u81F4\u3002"
lastmod: '2024-03-13T22:44:47.401070-06:00'
model: gpt-4-0125-preview
summary: "\u5C06\u5B57\u7B26\u4E32\u8F6C\u6362\u4E3A\u5C0F\u5199\u662F\u4E00\u79CD\
  \u57FA\u672C\u64CD\u4F5C\uFF0C\u6D89\u53CA\u5C06\u7ED9\u5B9A\u5B57\u7B26\u4E32\u4E2D\
  \u7684\u6240\u6709\u5B57\u7B26\u8F6C\u6362\u4E3A\u5176\u5C0F\u5199\u7B49\u4EF7\u7269\
  \u3002\u7A0B\u5E8F\u5458\u901A\u5E38\u6267\u884C\u6B64\u64CD\u4F5C\u4EE5\u5B9E\u73B0\
  \u5927\u5C0F\u5199\u4E0D\u654F\u611F\u7684\u6BD4\u8F83\u6216\u6807\u51C6\u5316\u6587\
  \u672C\u8F93\u5165\u4EE5\u4FBF\u8FDB\u4E00\u6B65\u5904\u7406\uFF0C\u4ECE\u800C\u4F7F\
  \u5E94\u7528\u7A0B\u5E8F\u66F4\u52A0\u7528\u6237\u53CB\u597D\uFF0C\u6570\u636E\u66F4\
  \u52A0\u4E00\u81F4\u3002"
title: "\u5C06\u5B57\u7B26\u4E32\u8F6C\u6362\u4E3A\u5C0F\u5199"
weight: 4
---

## 什么和为什么？

将字符串转换为小写是一种基本操作，涉及将给定字符串中的所有字符转换为其小写等价物。程序员通常执行此操作以实现大小写不敏感的比较或标准化文本输入以便进一步处理，从而使应用程序更加用户友好，数据更加一致。

## 如何操作：

在 Dart 中，您可以使用 `String` 类提供的 `toLowerCase()` 方法将字符串转换为小写。此方法返回一个新字符串，其中所有大写字符均已转换为小写。让我们通过一个简单的例子来看看这是如何工作的：

```dart
void main() {
  String originalString = "Hello, World!";
  String lowerCaseString = originalString.toLowerCase();

  print(lowerCaseString);  // 输出：hello, world!
}
```

Dart 不需要外部库即可执行基本的字符串操作任务，包括转换为小写，因为标准库的 `String` 类非常全面。然而，对于涉及特定区域规则的更复杂的操作，你可能会考虑使用 `intl` 包，该包提供国际化和本地化设施，包括基于区域设置的大小写转换：

要使用 `intl`，将其添加到您的 `pubspec.yaml` 文件中：

```yaml
dependencies:
  intl: ^0.17.0
```

然后，您可以使用 `toLocaleLowerCase()` 方法根据特定区域设置将字符串转换为小写：

```dart
import 'package:intl/intl.dart';

void main() {
  String originalString = "İstanbul";
  
  // 土耳其区域设置
  print(Intl.withLocale('tr', () => originalString.toLowerCase())); // 输出：istanbul
  
  // 默认区域设置 (en)
  print(originalString.toLowerCase()); // 输出：i̇stanbul
}
```

在此例中，请注意土耳其区域设置如何正确处理无点 'i'，展示了在国际化应用程序中区域感知转换的重要性。
