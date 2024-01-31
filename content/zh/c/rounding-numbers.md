---
title:                "数字取整"
date:                  2024-01-26T03:43:35.115566-07:00
model:                 gpt-4-0125-preview
simple_title:         "数字取整"

category:             "C"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c/rounding-numbers.md"
---

{{< edit_this_page >}}

## 数字舍入：是什么以及为何？
数字舍入是指切去某一点之后的数字，同时可选择性地调整最后保留的数字。程序员进行数字舍入是为了减少精度——当精确值不是必需的，管理浮点误差，或为了让数字更加用户友好地展示。

## 如何实现：
在C语言中，你会典型地使用 `floor()`，`ceil()` 或 `round()` 函数。这里是一个快速展示：

```C
#include <stdio.h>
#include <math.h>

int main() {
    double num = 3.14159;
    double num_floor = floor(num);
    double num_ceil = ceil(num);
    double num_round = round(num);

    printf("Floor: %.2f\n", num_floor); // Floor: 3.00
    printf("Ceil: %.2f\n", num_ceil);   // Ceil: 4.00
    printf("Round: %.2f\n", num_round); // Round: 3.00
    return 0;
}
```
为了更多控制，像是要舍入到一个特定的位置，你需要乘，舍入，然后除：

```C
double roundToPlace(double num, int place) {
    double scale = pow(10.0, place);
    return round(num * scale) / scale;
}

// ...

double num = 3.14159;
double num_rounded = roundToPlace(num, 2);
printf("Rounded to 2 decimal places: %.2f\n", num_rounded); // 舍入到2位小数点：3.14
```

## 深入了解
回到过去，数字舍入往往意味着一个手动过程——仅用笔和纸就是一项重大工作。有了计算机，我们自动化了这一过程，但浮点算术带来了一些细微之处，它的二进制性质导致某些数字无法精确表示。

标准舍入的替代方法包括截断（简单地丢弃额外的数字）或银行家舍入法，当数值正好处于两个值之间时，该方法舍入到最近的偶数，以减少重复计算中的偏差。

当你需要对任意精度的数字进行舍入或处理无穷大、信号NaN或非正规值等特殊情况时，实现就变得复杂。C标准库函数处理基础情况，但如果你需要以自定义方式舍入小数，你需要的不仅是`math.h`。

## 另请参见
- [`<math.h>` 文档](https://en.cppreference.com/w/c/numeric/math)
- [浮点算术](https://en.wikipedia.org/wiki/Floating-point_arithmetic)
- [验证浮点计算的陷阱](https://dl.acm.org/doi/10.1145/1186736.1186737)
