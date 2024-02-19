---
aliases:
- /zh/swift/rounding-numbers/
date: 2024-01-26 03:46:44.273121-07:00
description: "\u6570\u5B57\u56DB\u820D\u4E94\u5165\u610F\u5473\u7740\u5C06\u4E00\u4E2A\
  \u6570\u5B57\u503C\u8FD1\u4F3C\u5230\u7279\u5B9A\u7684\u7CBE\u5EA6\uFF0C\u901A\u5E38\
  \u662F\u4E3A\u4E86\u53BB\u9664\u4E0D\u9700\u8981\u7684\u5C0F\u6570\u3002\u7A0B\u5E8F\
  \u5458\u8FDB\u884C\u56DB\u820D\u4E94\u5165\u4EE5\u7BA1\u7406\u5185\u5B58\u3001\u63D0\
  \u9AD8\u53EF\u8BFB\u6027\uFF0C\u4EE5\u53CA\u6EE1\u8DB3\u7279\u5B9A\u9886\u57DF\u7684\
  \u9700\u6C42\uFF0C\u5982\u8D27\u5E01\u7EA6\u675F\u3002"
lastmod: 2024-02-18 23:08:59.437717
model: gpt-4-0125-preview
summary: "\u6570\u5B57\u56DB\u820D\u4E94\u5165\u610F\u5473\u7740\u5C06\u4E00\u4E2A\
  \u6570\u5B57\u503C\u8FD1\u4F3C\u5230\u7279\u5B9A\u7684\u7CBE\u5EA6\uFF0C\u901A\u5E38\
  \u662F\u4E3A\u4E86\u53BB\u9664\u4E0D\u9700\u8981\u7684\u5C0F\u6570\u3002\u7A0B\u5E8F\
  \u5458\u8FDB\u884C\u56DB\u820D\u4E94\u5165\u4EE5\u7BA1\u7406\u5185\u5B58\u3001\u63D0\
  \u9AD8\u53EF\u8BFB\u6027\uFF0C\u4EE5\u53CA\u6EE1\u8DB3\u7279\u5B9A\u9886\u57DF\u7684\
  \u9700\u6C42\uFF0C\u5982\u8D27\u5E01\u7EA6\u675F\u3002"
title: "\u6570\u5B57\u53D6\u6574"
---

{{< edit_this_page >}}

## 什么 & 为什么？

数字四舍五入意味着将一个数字值近似到特定的精度，通常是为了去除不需要的小数。程序员进行四舍五入以管理内存、提高可读性，以及满足特定领域的需求，如货币约束。

## 如何操作：

Swift 提供了几种四舍五入数字的方法。以下是一些示例：

```Swift
let original = 3.14159

// 标准四舍五入
let standardRounded = round(original) // 3.0

// 四舍五入到特定的小数位
let decimalRounded = Double(round(original * 1000) / 1000) // 3.142

// 向下四舍五入
let roundedDown = floor(original) // 3.0

// 向上四舍五入
let roundedUp = ceil(original) // 4.0

print("标准: \(standardRounded), 小数: \(decimalRounded), 向下: \(roundedDown), 向上: \(roundedUp)")
```

输出：`标准: 3.0, 小数: 3.142, 向下: 3.0, 向上: 4.0`

## 深入探讨

从历史上看，四舍五入是一种数学概念，早于计算机，对商业和科学至关重要。Swift 的 `Foundation` 框架提供了全面的四舍五入功能：

- `round(_: )`是传统的四舍五入法。
- `floor(_: )` 和 `ceil(_: )`处理方向四舍五入。
- `rounded(.up/.down/.toNearestOrAwayFromZero)` 通过四舍五入规则枚举提供了更精细的控制。

请注意 `Decimal` 类型用于精确的财务计算，它避免了浮点错误。同时，也探索 `NSDecimalNumber` 以实现与 Objective-C 的兼容性。

## 另请参阅

- 浮点运算的 IEEE 标准（IEEE 754）：[IEEE 754](https://ieeexplore.ieee.org/document/4610935)
