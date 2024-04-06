---
date: 2024-01-26 03:46:37.745026-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u56DB\u820D\u4E94\u5165\u5E76\u4E0D\u662F\
  \u65B0\u4E8B\u7269\u2014\u2014\u4EBA\u7C7B\u51E0\u4E2A\u4E16\u7EAA\u4EE5\u6765\u4E00\
  \u76F4\u5728\u8FDB\u884C\u56DB\u820D\u4E94\u5165\uFF0C\u4EE5\u4FBF\u7B80\u5316\u8BA1\
  \u7B97\u6216\u5728\u5176\u5DE5\u5177\u7684\u9650\u5236\u8303\u56F4\u5185\u5DE5\u4F5C\
  \u3002\u5728Ruby\u4E2D\uFF0C`round`\u65B9\u6CD5\u975E\u5E38\u7075\u6D3B\uFF0C\u80FD\
  \u591F\u9ED8\u8BA4\u56DB\u820D\u4E94\u5165\u5230\u6700\u8FD1\u7684\u6574\u6570\u6216\
  \u6307\u5B9A\u5C0F\u6570\u4F4D\u3002\u2026"
lastmod: '2024-04-05T22:38:47.504548-06:00'
model: gpt-4-0125-preview
summary: "`round`\u7684\u66FF\u4EE3\u65B9\u6CD5\u5305\u62EC\u603B\u662F\u5411\u4E0B\
  \u53D6\u6574\u7684`floor`\uFF0C\u548C\u65E0\u8BBA\u6570\u503C\u5982\u4F55\u603B\u662F\
  \u5411\u4E0A\u53D6\u6574\u7684`ceil`\u3002\u82E5\u4EC5\u60F3\u53BB\u6389\u5C0F\u6570\
  \u4F4D\uFF0C\u53EF\u4EE5\u4F7F\u7528`truncate`\u3002"
title: "\u6570\u5B57\u53D6\u6574"
weight: 13
---

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
