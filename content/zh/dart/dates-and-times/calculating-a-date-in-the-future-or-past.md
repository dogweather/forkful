---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:54:04.483735-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A Dart \u901A\u8FC7\u5176 `DateTime` \u7C7B\
  \u4E3A\u65E5\u671F\u64CD\u4F5C\u63D0\u4F9B\u4E86\u5F3A\u5927\u652F\u6301\u3002\u4EE5\
  \u4E0B\u662F\u5982\u4F55\u4F7F\u7528 Dart \u7684\u539F\u751F\u529F\u80FD\u8BA1\u7B97\
  \u672A\u6765\u6216\u8FC7\u53BB\u7684\u65E5\u671F\uFF0C\u65E0\u9700\u7B2C\u4E09\u65B9\
  \u5E93\u3002"
lastmod: '2024-04-05T21:53:47.764861-06:00'
model: gpt-4-0125-preview
summary: ''
title: "\u8BA1\u7B97\u672A\u6765\u6216\u8FC7\u53BB\u7684\u65E5\u671F"
weight: 26
---

## 如何操作：
Dart 通过其 `DateTime` 类为日期操作提供了强大支持。以下是如何使用 Dart 的原生功能计算未来或过去的日期，无需第三方库。

### 计算未来日期
要计算未来的日期，你需要创建一个 `DateTime` 对象并使用所需持续时间的 `add` 方法。

```dart
DateTime today = DateTime.now();
Duration tenDays = Duration(days: 10);
DateTime futureDate = today.add(tenDays);

print(futureDate); // 输出：2023-04-21 14:22:35.123456（示例输出，取决于当前日期和时间）
```

### 计算过去日期
为了计算过去的日期，你需要对 `DateTime` 对象使用必要持续时间的 `subtract` 方法。

```dart
DateTime today = DateTime.now();
Duration fifteenDaysAgo = Duration(days: 15);
DateTime pastDate = today.subtract(fifteenDaysAgo);

print(pastDate); // 输出：2023-03-27 14:22:35.123456（示例输出，取决于当前日期和时间）
```

### 使用第三方库
尽管 Dart 的原生日期操作能力很强大，但你可能会发现自己需要进行更具体的操作，例如更容易地解析或格式化日期，或执行复杂的计算。在这种情况下，`time` 包可能非常有用。

首先，在你的 `pubspec.yaml` 依赖中添加 `time`：

```yaml
dependencies:
  time: ^2.0.0
```

然后，你可以使用它来执行类似的计算，但可读性更强：

```dart
import 'package:time/time.dart';

void main() {
  DateTime today = DateTime.now();

  // 计算未来的日期
  DateTime futureDate = today + 10.days;
  print(futureDate); // 输出格式：2023-04-21 14:22:35.123456

  // 计算过去的日期
  DateTime pastDate = today - 15.days;
  print(pastDate); // 输出格式：2023-03-27 14:22:35.123456
}
```

这些示例展示了 Dart 中的基本日期操作，包括向当前日期添加和减去时间，演示了 Dart 应用程序中日期管理的轻松方式。
