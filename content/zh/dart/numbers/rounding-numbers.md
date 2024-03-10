---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:58.930987-07:00
description: "\u820D\u5165\u6570\u5B57\u662F\u5C06\u6570\u5B57\u8C03\u6574\u5230\u6700\
  \u63A5\u8FD1\u7684\u6574\u6570\u6216\u6307\u5B9A\u7684\u5C0F\u6570\u4F4D\u6570\u7684\
  \u8FC7\u7A0B\u3002\u7A0B\u5E8F\u5458\u7ECF\u5E38\u8FDB\u884C\u6570\u5B57\u820D\u5165\
  \u4EE5\u7B80\u5316\u8BA1\u7B97\u3001\u63D0\u9AD8\u53EF\u8BFB\u6027\u6216\u4E3A\u663E\
  \u793A\u6570\u636E\u505A\u51C6\u5907\uFF0C\u786E\u4FDD\u6570\u503C\u8F93\u51FA\u7684\
  \u4E00\u81F4\u6027\u548C\u6E05\u6670\u6027\u3002"
lastmod: '2024-03-09T21:06:11.737921-07:00'
model: gpt-4-0125-preview
summary: "\u820D\u5165\u6570\u5B57\u662F\u5C06\u6570\u5B57\u8C03\u6574\u5230\u6700\
  \u63A5\u8FD1\u7684\u6574\u6570\u6216\u6307\u5B9A\u7684\u5C0F\u6570\u4F4D\u6570\u7684\
  \u8FC7\u7A0B\u3002\u7A0B\u5E8F\u5458\u7ECF\u5E38\u8FDB\u884C\u6570\u5B57\u820D\u5165\
  \u4EE5\u7B80\u5316\u8BA1\u7B97\u3001\u63D0\u9AD8\u53EF\u8BFB\u6027\u6216\u4E3A\u663E\
  \u793A\u6570\u636E\u505A\u51C6\u5907\uFF0C\u786E\u4FDD\u6570\u503C\u8F93\u51FA\u7684\
  \u4E00\u81F4\u6027\u548C\u6E05\u6670\u6027\u3002"
title: "\u6570\u503C\u53D6\u820D"
---

{{< edit_this_page >}}

## 是什么 & 为什么？

舍入数字是将数字调整到最接近的整数或指定的小数位数的过程。程序员经常进行数字舍入以简化计算、提高可读性或为显示数据做准备，确保数值输出的一致性和清晰性。

## 如何执行：

Dart在其核心`num`类型中提供了原生的舍入操作方法。这里，我们将探索像`round()`、`floor()`、`ceil()`这样的方法，以及如何舍入到特定的小数位数。

### 舍入到最接近的整数：

```dart
var number = 3.56;
print(number.round()); // 输出：4
```

### 向下舍入：

```dart
print(number.floor()); // 输出：3
```

### 向上舍入：

```dart
print(number.ceil()); // 输出：4
```

### 舍入到特定的小数位数：

为了舍入到特定的小数位数，我们可以使用`toStringAsFixed()`方法，该方法返回一个字符串，或使用`dart:math`中的`pow`来得到数值结果。

```dart
import 'dart:math';

var number = 3.56789;
String roundedString = number.toStringAsFixed(2); // 用于显示目的
print(roundedString); // 输出：3.57

double roundedNumber = double.parse(roundedString);
print(roundedNumber); // 输出：3.57

// 或者，为了得到数值结果：
double roundedToDecimal = (number * pow(10, 2)).round().toDouble() / pow(10, 2);
print(roundedToDecimal); // 输出：3.57
```

虽然Dart的核心库有效地满足了大多数舍入需要，但对于更复杂的数学操作或精确的舍入要求，像`decimal`这样的库可能会很有用。`decimal`库提供了一种易于处理不失精度的十进制数字的方法，这对于金融计算尤其方便，但对于如展示的简单舍入方法而言，Dart的核心功能通常就足够了。
