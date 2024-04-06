---
date: 2024-01-26 03:46:19.471556-07:00
description: "\u600E\u4E48\u505A\uFF1A \u5728Python\u4E2D\uFF0C`round()` \u5E76\u4E0D\
  \u4EC5\u4EC5\u662F\u53BB\u6389\u5C0F\u6570\u90E8\u5206\u3002\u4ECE\u5386\u53F2\u4E0A\
  \u770B\uFF0CPython\u548C\u8BB8\u591A\u5176\u4ED6\u8BED\u8A00\u4E00\u6837\uFF0C\u9075\
  \u5FAA\u201C\u56DB\u820D\u516D\u5165\u4E94\u6210\u53CC\u201D\u6216\u201C\u94F6\u884C\
  \u5BB6\u820D\u5165\u6CD5\u201D\u3002\u8FD9\u79CD\u65B9\u6CD5\u5728\u91D1\u878D\u8BA1\
  \u7B97\u4E2D\u5F88\u91CD\u8981\uFF0C\u5B83\u6700\u5C0F\u5316\u4E86\u6C42\u548C\u6216\
  \u5E73\u5747\u503C\u4E2D\u7684\u7D2F\u79EF\u8BEF\u5DEE\u3002\u2026"
lastmod: '2024-04-05T22:51:00.477670-06:00'
model: gpt-4-0125-preview
summary: "\u82E5\u5BFB\u6C42\u66FF\u4EE3\u65B9\u6CD5\uFF0C\u53EF\u4EE5\u4F7F\u7528\
  Python\u7684\u6570\u5B66\u6A21\u5757\u4E2D\u7684`math.floor()`\u548C`math.ceil()`\uFF0C\
  \u5B83\u4EEC\u5C06\u6570\u5B57\u5411\u4E0B\u6216\u5411\u4E0A\u820D\u5165\u5230\u4E0B\
  \u4E00\u4E2A\u6574\u6570\u3002\u4F46\u5982\u679C\u4F60\u8FFD\u6C42\u7684\u662F\u7CBE\
  \u5EA6\uFF0C`decimal`\u6A21\u5757\u7684`quantize()`\u5141\u8BB8\u4F60\u6307\u5B9A\
  \u820D\u5165\u884C\u4E3A\u3002"
title: "\u6570\u5B57\u53D6\u6574"
weight: 13
---

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
