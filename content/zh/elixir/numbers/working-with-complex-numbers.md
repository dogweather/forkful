---
date: 2024-01-26 04:39:16.428546-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A Elixir\u6CA1\u6709\u5185\u7F6E\u7684\u590D\
  \u6570\u652F\u6301\uFF0C\u56E0\u6B64\u6211\u4EEC\u9700\u8981\u81EA\u5DF1\u5B9E\u73B0\
  \u6216\u4F7F\u7528\u5E93\uFF0C\u6BD4\u5982 `ComplexNum`\u3002\u8FD9\u91CC\u6709\u4E00\
  \u4E2A\u4F7F\u7528\u5E93\u7684\u7B80\u77ED\u793A\u4F8B\uFF1A."
lastmod: '2024-04-05T22:38:46.527167-06:00'
model: gpt-4-0125-preview
summary: "\u5982\u4F55\u64CD\u4F5C\uFF1A Elixir\u6CA1\u6709\u5185\u7F6E\u7684\u590D\
  \u6570\u652F\u6301\uFF0C\u56E0\u6B64\u6211\u4EEC\u9700\u8981\u81EA\u5DF1\u5B9E\u73B0\
  \u6216\u4F7F\u7528\u5E93\uFF0C\u6BD4\u5982 `ComplexNum`\u3002\u8FD9\u91CC\u6709\u4E00\
  \u4E2A\u4F7F\u7528\u5E93\u7684\u7B80\u77ED\u793A\u4F8B\uFF1A."
title: "\u5904\u7406\u590D\u6570"
weight: 14
---

## 如何操作：
Elixir没有内置的复数支持，因此我们需要自己实现或使用库，比如 `ComplexNum`。这里有一个使用库的简短示例：

```elixir
# 假设你已经安装了ComplexNum
defmodule ComplexMath do
  import ComplexNum

  def add(a, b) do
    ComplexNum.add(a, b)
  end
end

# 创建复数并将它们相加
c1 = {3, 4}   # 代表 3 + 4i
c2 = {2, -3}  # 代表 2 - 3i
result = ComplexMath.add(c1, c2)
IO.puts "结果是: #{inspect(result)}"
```

这将输出：
```
结果是: {5, 1}
```

这意味着 `3 + 4i` 和 `2 - 3i` 的和是 `5 + 1i`。

## 深入探讨
复数之所以出现在历史上，是因为普通的老数字无法处理负数的平方根。直到17世纪，多亏了数学家如René Descartes和Gerolamo Cardano，复数才被认真对待。

在Elixir中，你通常使用元组如 `{3, 4}` 来表示复数，或使用专用库以避免重新发明轮子。库通常更好——它们处理了一些棘手的问题，如乘法和除法，这些问题因虚数单位 'i'（提示：`i` 的平方等于 `-1`）而变得复杂。

## 另请参阅
查看这些资源：
- [ComplexNum 库](https://hex.pm/packages/complex_num)，Elixir的包管理器Hex。
- [Elixir School](https://elixirschool.com/en/)，用于高级Elixir主题和练习。
- [Erlang -- math 模块](http://erlang.org/doc/man/math.html)，Elixir在底层使用的，用于其它数学需求。
