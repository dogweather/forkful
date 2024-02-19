---
aliases:
- /zh/elixir/rounding-numbers/
date: 2024-01-26 03:43:57.567480-07:00
description: "\u56DB\u820D\u4E94\u5165\u6570\u5B57\u610F\u5473\u7740\u5C06\u5B83\u4EEC\
  \u8C03\u6574\u81F3\u4E00\u4E2A\u8FD1\u4F3C\u503C\u4EE5\u7B80\u5316\u6216\u5339\u914D\
  \u7279\u5B9A\u7684\u7CBE\u5EA6\u3002\u8FD9\u5BF9\u4E8E\u63D0\u9AD8\u53EF\u8BFB\u6027\
  \u3001\u51CF\u5C11\u5B58\u50A8\u7A7A\u95F4\u6216\u6EE1\u8DB3\u7279\u5B9A\u9886\u57DF\
  \u7684\u9700\u6C42\u5F88\u6709\u7528\uFF0C\u6BD4\u5982\u8D27\u5E01\u8BA1\u7B97\uFF0C\
  \u4F60\u60F3\u8981\u5C06\u5176\u56DB\u820D\u4E94\u5165\u5230\u6700\u63A5\u8FD1\u7684\
  \u5206\u3002"
lastmod: 2024-02-18 23:08:58.861479
model: gpt-4-0125-preview
summary: "\u56DB\u820D\u4E94\u5165\u6570\u5B57\u610F\u5473\u7740\u5C06\u5B83\u4EEC\
  \u8C03\u6574\u81F3\u4E00\u4E2A\u8FD1\u4F3C\u503C\u4EE5\u7B80\u5316\u6216\u5339\u914D\
  \u7279\u5B9A\u7684\u7CBE\u5EA6\u3002\u8FD9\u5BF9\u4E8E\u63D0\u9AD8\u53EF\u8BFB\u6027\
  \u3001\u51CF\u5C11\u5B58\u50A8\u7A7A\u95F4\u6216\u6EE1\u8DB3\u7279\u5B9A\u9886\u57DF\
  \u7684\u9700\u6C42\u5F88\u6709\u7528\uFF0C\u6BD4\u5982\u8D27\u5E01\u8BA1\u7B97\uFF0C\
  \u4F60\u60F3\u8981\u5C06\u5176\u56DB\u820D\u4E94\u5165\u5230\u6700\u63A5\u8FD1\u7684\
  \u5206\u3002"
title: "\u6570\u5B57\u53D6\u6574"
---

{{< edit_this_page >}}

## 什么和为什么？
四舍五入数字意味着将它们调整至一个近似值以简化或匹配特定的精度。这对于提高可读性、减少存储空间或满足特定领域的需求很有用，比如货币计算，你想要将其四舍五入到最接近的分。

## 如何操作：
在Elixir中，你可以使用 `Float.round/2` 来对浮点数进行四舍五入。你可以指定想要保留的小数位数。这是它的工作原理：

```elixir
# 将数字四舍五入到无小数位
Float.round(3.14159) # => 3.0

# 将数字四舍五入到2个小数位
Float.round(3.14159, 2) # => 3.14

# 将数字四舍五入到负精度至最近的10
Float.round(123.456, -1) # => 120.0
```

## 深入探究
在计算机科学中，四舍五入数字是一个经典问题——以至于选择哪种四舍五入策略可以影响财务系统、科学计算等。Elixir的 `Float.round/2` 默认为“四舍五入”策略，类似于在数学课上教的传统四舍五入。

如果你需要其他类型的四舍五入，Elixir允许你自己实现。例如，“向下取整”(总是向下)或者“向上取整”(总是向上)的四舍五入。分别使用 `Float.floor/1` 或 `Float.ceil/1`。

```elixir
# 向下取整
Float.floor(3.999) # => 3.0

# 向上取整
Float.ceil(3.001) # => 4.0
```

这些替代方法有助于根据你的应用需求量身定做四舍五入，无论是财务计算、图形渲染还是数据近似。

## 另请参阅
有关Elixir的四舍五入函数和浮点数的更多信息：

- Elixir官方文档中的`Float`：https://hexdocs.pm/elixir/Float.html
- 浮点数算术的IEEE标准 (IEEE 754)：https://ieeexplore.ieee.org/document/4610935
