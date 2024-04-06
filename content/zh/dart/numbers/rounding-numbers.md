---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:58.930987-07:00
description: "\u5982\u4F55\u6267\u884C\uFF1A Dart\u5728\u5176\u6838\u5FC3`num`\u7C7B\
  \u578B\u4E2D\u63D0\u4F9B\u4E86\u539F\u751F\u7684\u820D\u5165\u64CD\u4F5C\u65B9\u6CD5\
  \u3002\u8FD9\u91CC\uFF0C\u6211\u4EEC\u5C06\u63A2\u7D22\u50CF`round()`\u3001`floor()`\u3001\
  `ceil()`\u8FD9\u6837\u7684\u65B9\u6CD5\uFF0C\u4EE5\u53CA\u5982\u4F55\u820D\u5165\
  \u5230\u7279\u5B9A\u7684\u5C0F\u6570\u4F4D\u6570\u3002"
lastmod: '2024-04-05T22:38:46.574072-06:00'
model: gpt-4-0125-preview
summary: "\u5982\u4F55\u6267\u884C\uFF1A Dart\u5728\u5176\u6838\u5FC3`num`\u7C7B\u578B\
  \u4E2D\u63D0\u4F9B\u4E86\u539F\u751F\u7684\u820D\u5165\u64CD\u4F5C\u65B9\u6CD5\u3002\
  \u8FD9\u91CC\uFF0C\u6211\u4EEC\u5C06\u63A2\u7D22\u50CF`round()`\u3001`floor()`\u3001\
  `ceil()`\u8FD9\u6837\u7684\u65B9\u6CD5\uFF0C\u4EE5\u53CA\u5982\u4F55\u820D\u5165\
  \u5230\u7279\u5B9A\u7684\u5C0F\u6570\u4F4D\u6570\u3002"
title: "\u6570\u503C\u53D6\u820D"
weight: 13
---

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
