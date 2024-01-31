---
title:                "数字取整"
date:                  2024-01-26T03:46:37.745026-07:00
model:                 gpt-4-0125-preview
simple_title:         "数字取整"

category:             "Ruby"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/ruby/rounding-numbers.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？
四舍五入意味着将数字调整到最近的整数或指定的精度程度。程序员四舍五入数字是为了简化，符合人类的期望，或使数据适应特定的格式——比如财务计算、图形显示或减少存储大小。

## 如何操作：

```Ruby
# 基本四舍五入
puts 3.14159.round      # => 3
puts 2.6.round          # => 3

# 指定精度
puts 3.14159.round(2)   # => 3.14
puts 2.675.round(2)     # => 2.68

# 向下取整
puts 2.9.floor          # => 2

# 向上取整
puts 2.1.ceil           # => 3

# 向零方向取整
puts -2.9.round         # => -3
puts -2.9.truncate      # => -2
```

示例输出：
```
3
3
3.14
2.68
2
3
-3
-2
```

## 深入探讨
四舍五入并不是新事物——人类几个世纪以来一直在进行四舍五入，以便简化计算或在其工具的限制范围内工作。在Ruby中，`round`方法非常灵活，能够默认四舍五入到最近的整数或指定小数位。

`round`的替代方法包括总是向下取整的`floor`，和无论数值如何总是向上取整的`ceil`。若仅想去掉小数位，可以使用`truncate`。

历史上，在计算机领域，四舍五入在处理浮点运算时变得至关重要，因为浮点运算本身就不精确。像大多数语言一样，Ruby遵循IEEE 754标准来处理浮点数，这意味着它的四舍五入方式大多数程序员应能预测并依赖。

不过，还有更多要注意的——比如银行家舍入法（也称为向偶数舍入）等概念，Ruby开发者可能需要手动实现，因为`round`方法内置并不提供这个功能。

## 另见
- [Ruby 文档](https://ruby-doc.org/core-3.0.0/Float.html#method-i-round)中关于Floats的`round`方法。
- [IEEE浮点运算标准 (IEEE 754)](https://ieeexplore.ieee.org/document/4610935)。
- [理解浮点精度](https://floating-point-gui.de/)，深入了解计算机如何处理十进制数。
