---
date: 2024-01-26 03:46:19.471556-07:00
description: "\u5C06\u6570\u5B57\u56DB\u820D\u4E94\u5165\u610F\u5473\u7740\u5C06\u5B83\
  \u4EEC\u8C03\u6574\u5F97\u66F4\u63A5\u8FD1\u4E00\u4E2A\u66F4\u7B80\u5355\u6216\u66F4\
  \u6709\u610F\u4E49\u7684\u503C\u3002\u7A0B\u5E8F\u5458\u56DB\u820D\u4E94\u5165\u6570\
  \u5B57\u662F\u4E3A\u4E86\u7B80\u5316\u7ED3\u679C\u3001\u9650\u5236\u663E\u793A\u7684\
  \u5C0F\u6570\u4F4D\u6570\u6216\u67D0\u4E9B\u6570\u5B66\u76EE\u7684\u3002"
lastmod: '2024-03-13T22:44:47.249386-06:00'
model: gpt-4-0125-preview
summary: "\u5C06\u6570\u5B57\u56DB\u820D\u4E94\u5165\u610F\u5473\u7740\u5C06\u5B83\
  \u4EEC\u8C03\u6574\u5F97\u66F4\u63A5\u8FD1\u4E00\u4E2A\u66F4\u7B80\u5355\u6216\u66F4\
  \u6709\u610F\u4E49\u7684\u503C\u3002\u7A0B\u5E8F\u5458\u56DB\u820D\u4E94\u5165\u6570\
  \u5B57\u662F\u4E3A\u4E86\u7B80\u5316\u7ED3\u679C\u3001\u9650\u5236\u663E\u793A\u7684\
  \u5C0F\u6570\u4F4D\u6570\u6216\u67D0\u4E9B\u6570\u5B66\u76EE\u7684\u3002"
title: "\u6570\u5B57\u53D6\u6574"
weight: 13
---

## 什么&为什么？
将数字四舍五入意味着将它们调整得更接近一个更简单或更有意义的值。程序员四舍五入数字是为了简化结果、限制显示的小数位数或某些数学目的。

## 怎么做：
以下是在Python中四舍五入数字的详细说明：

```python
# 将数字四舍五入到最接近的整数
print(round(8.67))  # 输出：9

# 将数字四舍五入到指定的小数位数
print(round(8.67, 1))  # 输出：8.7

# 当数字位于中间位置时，偶数向下四舍五入，奇数向上四舍五入
print(round(2.5))  # 输出：2
print(round(3.5))  # 输出：4
```

## 深入了解
在Python中，`round()` 并不仅仅是去掉小数部分。从历史上看，Python和许多其他语言一样，遵循“四舍六入五成双”或“银行家舍入法”。这种方法在金融计算中很重要，它最小化了求和或平均值中的累积误差。

若寻求替代方法，可以使用Python的数学模块中的`math.floor()`和`math.ceil()`，它们将数字向下或向上舍入到下一个整数。但如果你追求的是精度，`decimal`模块的`quantize()`允许你指定舍入行为。

底层来看，`round()`处理的是二进制浮点数。由于某些小数在二进制中无法精确表达，你可能会对诸如`round(2.675, 2)`没有像预期那样变成`2.68`这类情况感到惊讶。此时可以考虑使用`decimal`或`fractions`来实现高精度。

## 另见
- Python的内置函数文档：https://docs.python.org/3/library/functions.html#round
- Decimal定点数和浮点数算术：https://docs.python.org/3/library/decimal.html
- Python的数学模块：https://docs.python.org/3/library/math.html
