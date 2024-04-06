---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:53:54.965657-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u5728 Dart \u4E2D\uFF0C\u60A8\u53EF\u4EE5\
  \u4F7F\u7528 `String` \u7C7B\u63D0\u4F9B\u7684 `toLowerCase()` \u65B9\u6CD5\u5C06\
  \u5B57\u7B26\u4E32\u8F6C\u6362\u4E3A\u5C0F\u5199\u3002\u6B64\u65B9\u6CD5\u8FD4\u56DE\
  \u4E00\u4E2A\u65B0\u5B57\u7B26\u4E32\uFF0C\u5176\u4E2D\u6240\u6709\u5927\u5199\u5B57\
  \u7B26\u5747\u5DF2\u8F6C\u6362\u4E3A\u5C0F\u5199\u3002\u8BA9\u6211\u4EEC\u901A\u8FC7\
  \u4E00\u4E2A\u7B80\u5355\u7684\u4F8B\u5B50\u6765\u770B\u770B\u8FD9\u662F\u5982\u4F55\
  \u5DE5\u4F5C\u7684\uFF1A."
lastmod: '2024-04-05T21:53:47.732148-06:00'
model: gpt-4-0125-preview
summary: ''
title: "\u5C06\u5B57\u7B26\u4E32\u8F6C\u6362\u4E3A\u5C0F\u5199"
weight: 4
---

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
