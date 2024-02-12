---
title:                "数字取整"
aliases:
- zh/elixir/rounding-numbers.md
date:                  2024-01-26T03:43:57.567480-07:00
model:                 gpt-4-0125-preview
simple_title:         "数字取整"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elixir/rounding-numbers.md"
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
