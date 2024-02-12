---
title:                "数字取整"
aliases: - /zh/swift/rounding-numbers.md
date:                  2024-01-26T03:46:44.273121-07:00
model:                 gpt-4-0125-preview
simple_title:         "数字取整"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/swift/rounding-numbers.md"
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
