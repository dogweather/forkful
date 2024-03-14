---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:54:04.483735-07:00
description: "\u5BF9\u7A0B\u5E8F\u5458\u6765\u8BF4\uFF0C\u8BA1\u7B97\u672A\u6765\u6216\
  \u8FC7\u53BB\u7684\u65E5\u671F\u662F\u4E00\u9879\u5E38\u89C1\u4EFB\u52A1\uFF0C\u6D89\
  \u53CA\u5230\u6392\u7A0B\u3001\u63D0\u9192\u6216\u4EFB\u4F55\u4F9D\u8D56\u4E8E\u65E5\
  \u671F\u8BA1\u7B97\u7684\u529F\u80FD\u3002\u7406\u89E3\u5982\u4F55\u64CD\u4F5C\u65E5\
  \u671F\u5BF9\u4E8E\u540E\u7AEF\u7CFB\u7EDF\u3001\u7528\u6237\u754C\u9762\u548C\u6570\
  \u636E\u5206\u6790\u81F3\u5173\u91CD\u8981\uFF0C\u7279\u522B\u662F\u5BF9\u4E8E\u90A3\
  \u4E9B\u8FC7\u6E21\u5230 Dart \u4E14\u5E0C\u671B\u6709\u6548\u5B9E\u73B0\u65F6\u95F4\
  \u903B\u8F91\u7684\u4EBA\u6765\u8BF4\u3002"
lastmod: '2024-03-13T22:44:47.436615-06:00'
model: gpt-4-0125-preview
summary: "\u5BF9\u7A0B\u5E8F\u5458\u6765\u8BF4\uFF0C\u8BA1\u7B97\u672A\u6765\u6216\
  \u8FC7\u53BB\u7684\u65E5\u671F\u662F\u4E00\u9879\u5E38\u89C1\u4EFB\u52A1\uFF0C\u6D89\
  \u53CA\u5230\u6392\u7A0B\u3001\u63D0\u9192\u6216\u4EFB\u4F55\u4F9D\u8D56\u4E8E\u65E5\
  \u671F\u8BA1\u7B97\u7684\u529F\u80FD\u3002\u7406\u89E3\u5982\u4F55\u64CD\u4F5C\u65E5\
  \u671F\u5BF9\u4E8E\u540E\u7AEF\u7CFB\u7EDF\u3001\u7528\u6237\u754C\u9762\u548C\u6570\
  \u636E\u5206\u6790\u81F3\u5173\u91CD\u8981\uFF0C\u7279\u522B\u662F\u5BF9\u4E8E\u90A3\
  \u4E9B\u8FC7\u6E21\u5230 Dart \u4E14\u5E0C\u671B\u6709\u6548\u5B9E\u73B0\u65F6\u95F4\
  \u903B\u8F91\u7684\u4EBA\u6765\u8BF4\u3002"
title: "\u8BA1\u7B97\u672A\u6765\u6216\u8FC7\u53BB\u7684\u65E5\u671F"
---

{{< edit_this_page >}}

## 什么 & 为什么？
对程序员来说，计算未来或过去的日期是一项常见任务，涉及到排程、提醒或任何依赖于日期计算的功能。理解如何操作日期对于后端系统、用户界面和数据分析至关重要，特别是对于那些过渡到 Dart 且希望有效实现时间逻辑的人来说。

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
