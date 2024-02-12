---
title:                "数字取整"
aliases:
- /zh/cpp/rounding-numbers/
date:                  2024-01-26T03:44:39.958014-07:00
model:                 gpt-4-0125-preview
simple_title:         "数字取整"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/rounding-numbers.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？
数字四舍五入意味着将一个值调整到最近的整数或指定的精度。开发人员进行四舍五入操作是为了简化、符合现实世界的限制，或通过丢弃多余的精度来提高性能。

## 如何实现：
C++提供了几种四舍五入数字的方法，比如`floor()`、`ceil()`和`round()`：

```C++
#include <iostream>
#include <cmath> // 用于四舍五入的函数

int main() {
    double num = 3.14;

    std::cout << "floor: " << std::floor(num) << "\n"; // 输出: floor: 3
    std::cout << "ceil: " << std::ceil(num) << "\n";   // 输出: ceil: 4
    std::cout << "round: " << std::round(num) << "\n"; // 输出: round: 3

    // 对于固定精度的四舍五入，例如四舍五入到两位小数：
    double precise_num = 3.146;
    double multiplier = 100.0;
    double rounded = std::round(precise_num * multiplier) / multiplier;

    std::cout << "四舍五入到两位小数: " << rounded << "\n"; // 输出: 四舍五入到两位小数: 3.15

    return 0;
}
```

## 深入了解
在C++11之前，四舍五入依赖于手工技术或非标准库。今天，`<cmath>`提供了健壮的方法。`floor()`向下取整，`ceil()`向上取整，而`round()`则取最近的整数，甚至处理平局（0.5的情况）时通过取偶数来四舍五入。

了解这些函数的行为至关重要；例如，负数可能会让你措手不及（`std::round(-2.5)` 产生 `-2.0`）。

有什么替代方案吗？对正数先加0.5然后转换为int是一个经典技巧，但它对负数不起作用，而且不适用于所有类型。像Boost这样的库可以提供更细微的方法，而语言扩展或编译器内置函数可以针对特定硬件进行优化。

## 另请参阅
- C++ `<cmath>`参考资料：https://en.cppreference.com/w/cpp/header/cmath
- 浮点算术的IEEE标准（IEEE 754）：https://ieeexplore.ieee.org/document/4610935
- Boost数值转换库：https://www.boost.org/doc/libs/release/libs/numeric/conversion/
