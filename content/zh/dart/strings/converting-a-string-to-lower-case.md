---
title:                "将字符串转换为小写"
date:                  2024-03-08T21:53:54.965657-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

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
