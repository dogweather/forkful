---
date: 2024-01-26 01:17:42.742148-07:00
description: "\u91CD\u6784\u662F\u5728\u4E0D\u6539\u53D8\u73B0\u6709\u4EE3\u7801\u5916\
  \u90E8\u884C\u4E3A\u7684\u60C5\u51B5\u4E0B\u5BF9\u5176\u8FDB\u884C\u91CD\u7EC4\u7684\
  \u8FC7\u7A0B\uFF0C\u65E8\u5728\u6539\u5584\u53EF\u8BFB\u6027\u548C\u53EF\u7EF4\u62A4\
  \u6027\u7B49\u975E\u529F\u80FD\u5C5E\u6027\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\
  \u662F\u4E3A\u4E86\u4F7F\u4EE3\u7801\u66F4\u5E72\u51C0\u3001\u66F4\u6613\u4E8E\u7406\
  \u89E3\u548C\u66F4\u9AD8\u6548\uFF0C\u6709\u52A9\u4E8E\u672A\u6765\u7684\u66F4\u65B0\
  \uFF0C\u5E76\u964D\u4F4EBug\u7684\u98CE\u9669\u3002"
lastmod: '2024-02-25T18:49:44.995885-07:00'
model: gpt-4-0125-preview
summary: "\u91CD\u6784\u662F\u5728\u4E0D\u6539\u53D8\u73B0\u6709\u4EE3\u7801\u5916\
  \u90E8\u884C\u4E3A\u7684\u60C5\u51B5\u4E0B\u5BF9\u5176\u8FDB\u884C\u91CD\u7EC4\u7684\
  \u8FC7\u7A0B\uFF0C\u65E8\u5728\u6539\u5584\u53EF\u8BFB\u6027\u548C\u53EF\u7EF4\u62A4\
  \u6027\u7B49\u975E\u529F\u80FD\u5C5E\u6027\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\
  \u662F\u4E3A\u4E86\u4F7F\u4EE3\u7801\u66F4\u5E72\u51C0\u3001\u66F4\u6613\u4E8E\u7406\
  \u89E3\u548C\u66F4\u9AD8\u6548\uFF0C\u6709\u52A9\u4E8E\u672A\u6765\u7684\u66F4\u65B0\
  \uFF0C\u5E76\u964D\u4F4EBug\u7684\u98CE\u9669\u3002"
title: "\u4EE3\u7801\u91CD\u6784"
---

{{< edit_this_page >}}

## 什么 & 为什么？
重构是在不改变现有代码外部行为的情况下对其进行重组的过程，旨在改善可读性和可维护性等非功能属性。程序员这样做是为了使代码更干净、更易于理解和更高效，有助于未来的更新，并降低Bug的风险。

## 如何进行：
让我们整理一个常见的Elixir模式。我们将通过将一个功能`calculate_stats`拆分为更小、可重用的部分来重构它。

```elixir
defmodule Stats do
  # 原始的、未重构的代码
  def calculate_stats(data) do
    total = Enum.sum(data)
    count = Enum.count(data)
    mean = total / count
    {mean, total}
  end
  
  # 重构后的代码
  def calculate_mean(data), do: Enum.sum(data) / Enum.count(data)
  
  def calculate_total(data), do: Enum.sum(data)
  
  def calculate_stats_refactored(data) do
    mean = calculate_mean(data)
    total = calculate_total(data)
    {mean, total}
  end
end

# 示例输出
# 重构前
Stats.calculate_stats([1, 2, 3])
# => {2.0, 6}

# 重构后
Stats.calculate_stats_refactored([1, 2, 3])
# => {2.0, 6}
```
如您所见，输出保持不变，但现在我们拥有可重用和独立测试的模块化函数。

## 深入探讨
重构并不是一个新概念；自软件开发早期以来，它就一直是编程的重要部分。如Martin Fowler的《重构：改善既有代码的设计》等著名作品，为重构提供了基础实践，并提供了何时以及如何应用它们的见解。

手动重构的替代品包括自动化代码分析工具，这些工具可以建议甚至执行重构。然而，自动化工具可能并不总是能够完全理解代码的全文和细节，可能会错过人类审阅者会捕捉到的微妙之处。

在Elixir中具体的实施细节包括理解函数式范例并利用模式匹配、守卫子句和管道操作符来编写清晰、简洁的代码。例如，重构通常涉及将复杂的命令式函数转换为更小的、可组合的函数，这些函数遵循Elixir对不可变性和无副作用操作的偏好。

## 另请参阅
有关Elixir具体重构技术的更多信息：

- [Elixir的官方指南](https://elixir-lang.org/getting-started/)
- [《重构：改善既有代码的设计》by Martin Fowler](https://martinfowler.com/books/refactoring.html)，可应用于Elixir的一般原则。
- [Credo，Elixir的静态代码分析工具](https://github.com/rrrene/credo)，鼓励最佳实践。
- [Exercism Elixir Track](https://exercism.org/tracks/elixir)，提供经常涉及重构的实践练习。
