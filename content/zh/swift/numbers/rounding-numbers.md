---
date: 2024-01-26 03:46:44.273121-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u4ECE\u5386\u53F2\u4E0A\u770B\uFF0C\u56DB\
  \u820D\u4E94\u5165\u662F\u4E00\u79CD\u6570\u5B66\u6982\u5FF5\uFF0C\u65E9\u4E8E\u8BA1\
  \u7B97\u673A\uFF0C\u5BF9\u5546\u4E1A\u548C\u79D1\u5B66\u81F3\u5173\u91CD\u8981\u3002\
  Swift \u7684 `Foundation` \u6846\u67B6\u63D0\u4F9B\u4E86\u5168\u9762\u7684\u56DB\
  \u820D\u4E94\u5165\u529F\u80FD\uFF1A - `round(_: )`\u662F\u4F20\u7EDF\u7684\u56DB\
  \u820D\u4E94\u5165\u6CD5\u3002 - `floor(_: )` \u548C `ceil(_: )`\u5904\u7406\u65B9\
  \u5411\u56DB\u820D\u4E94\u5165\u3002 -\u2026"
lastmod: '2024-04-05T22:51:01.359919-06:00'
model: gpt-4-0125-preview
summary: ")`\u662F\u4F20\u7EDF\u7684\u56DB\u820D\u4E94\u5165\u6CD5\u3002 - `floor(_:\
  \ )` \u548C `ceil(_: )`\u5904\u7406\u65B9\u5411\u56DB\u820D\u4E94\u5165\u3002 -\
  \ `rounded(.up/.down/.toNearestOrAwayFromZero)` \u901A\u8FC7\u56DB\u820D\u4E94\u5165\
  \u89C4\u5219\u679A\u4E3E\u63D0\u4F9B\u4E86\u66F4\u7CBE\u7EC6\u7684\u63A7\u5236\u3002\
  \ \u8BF7\u6CE8\u610F `Decimal` \u7C7B\u578B\u7528\u4E8E\u7CBE\u786E\u7684\u8D22\u52A1\
  \u8BA1\u7B97\uFF0C\u5B83\u907F\u514D\u4E86\u6D6E\u70B9\u9519\u8BEF\u3002\u540C\u65F6\
  \uFF0C\u4E5F\u63A2\u7D22 `NSDecimalNumber` \u4EE5\u5B9E\u73B0\u4E0E Objective-C\
  \ \u7684\u517C\u5BB9\u6027\u3002"
title: "\u6570\u5B57\u53D6\u6574"
weight: 13
---

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
