---
title:                "数字取整"
date:                  2024-01-26T03:44:36.820140-07:00
model:                 gpt-4-0125-preview
simple_title:         "数字取整"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/gleam/rounding-numbers.md"
---

{{< edit_this_page >}}

## 什么与为什么?
数字四舍五入是指将一个值调整为最接近的指定位数值——比如，如果我们将数字四舍五入到整数，那么2.56将变成3。程序员这样做是为了简化问题或满足某些数值规范，通常是为了避免由浮点精度误差引起的细微差别或使输出对用户友好。

## 如何操作：
在Gleam中，截至我最后一次检查，四舍五入不在标准库中，但以下是直接使用Erlang函数将浮点数四舍五入到最接近的整数的典型方法：

```gleam
external fn erlang_round(Float) -> Int = "erlang" "round"

pub fn main() {
  let rounded = erlang_round(2.56)
  rounded // 输出: 3
}
```

输出：
```
3
```

有不同的精度要求吗？比如，四舍五入到两个小数位？我们需要一点数学知识：

```gleam
pub fn round_to_two_places(num: Float) -> Float {
  let multiplier = 100.0
  let tmp = num * multiplier
  let round_tmp = erlang_round(tmp)
  round_tmp / multiplier
}

pub fn main() {
    round_to_two_places(2.569) // 输出: 2.57
}
```

输出：
```
2.57
```

## 深入探究
从历史上看，四舍五入数字一直至关重要，特别是在金融和科学计算中，准确性和标准至关重要。没有四舍五入，你将得到到处都是难看的长小数，这会使得计算变得不切实际且容易出错。

在编程世界中，不同的语言提供不同的方法，从内置函数到全面的数学库。四舍五入可能涉及不同的规则——例如，“向上四舍五入”（通常的方法）或“向偶数四舍五入”（在金融计算中经常使用，以避免偏差）。

Gleam作为一种源于Erlang的年轻语言，依赖于Erlang的强大数值函数集。随着语言的发展，我们可能会看到引入原生函数，减少调用外部程序的需要。

## 另见
- Erlang的:math 模块，了解更多数字运算：https://erlang.org/doc/man/math.html
- 为什么四舍五入可能会变得棘手，IEEE 浮点标准：https://ieeexplore.ieee.org/document/8766229
- 对此背后的数学感兴趣吗？查看“每个计算机科学家应该了解的关于浮点算术的知识”：https://docs.oracle.com/cd/E19957-01/806-3568/ncg_goldberg.html
