---
aliases:
- /zh/ruby/refactoring/
date: 2024-01-26 03:36:38.019447-07:00
description: "\u91CD\u6784\u662F\u6307\u5728\u4E0D\u6539\u53D8\u73B0\u6709\u8BA1\u7B97\
  \u673A\u4EE3\u7801\u5916\u90E8\u884C\u4E3A\u7684\u60C5\u51B5\u4E0B\uFF0C\u91CD\u65B0\
  \u7EC4\u7EC7\u7ED3\u6784\u7684\u8FC7\u7A0B\u3002\u7A0B\u5E8F\u5458\u8FDB\u884C\u91CD\
  \u6784\u4EE5\u6539\u5584\u8F6F\u4EF6\u7684\u975E\u529F\u80FD\u5C5E\u6027\uFF0C\u5982\
  \u53EF\u8BFB\u6027\u3001\u964D\u4F4E\u590D\u6742\u6027\u3001\u6539\u5584\u53EF\u7EF4\
  \u62A4\u6027\u6216\u6027\u80FD\u589E\u5F3A\u3002"
lastmod: 2024-02-18 23:08:59.608251
model: gpt-4-0125-preview
summary: "\u91CD\u6784\u662F\u6307\u5728\u4E0D\u6539\u53D8\u73B0\u6709\u8BA1\u7B97\
  \u673A\u4EE3\u7801\u5916\u90E8\u884C\u4E3A\u7684\u60C5\u51B5\u4E0B\uFF0C\u91CD\u65B0\
  \u7EC4\u7EC7\u7ED3\u6784\u7684\u8FC7\u7A0B\u3002\u7A0B\u5E8F\u5458\u8FDB\u884C\u91CD\
  \u6784\u4EE5\u6539\u5584\u8F6F\u4EF6\u7684\u975E\u529F\u80FD\u5C5E\u6027\uFF0C\u5982\
  \u53EF\u8BFB\u6027\u3001\u964D\u4F4E\u590D\u6742\u6027\u3001\u6539\u5584\u53EF\u7EF4\
  \u62A4\u6027\u6216\u6027\u80FD\u589E\u5F3A\u3002"
title: "\u91CD\u6784"
---

{{< edit_this_page >}}

## 什么 & 为什么？

重构是指在不改变现有计算机代码外部行为的情况下，重新组织结构的过程。程序员进行重构以改善软件的非功能属性，如可读性、降低复杂性、改善可维护性或性能增强。

## 如何进行：

让我们通过重构一个计算平方和的 Ruby 方法的例子来说明。

**重构前:**
```ruby
def sum_of_squares(numbers)
  sum = 0
  numbers.each do |number|
    square = number * number
    sum += square
  end
  sum
end

puts sum_of_squares([1, 2, 3])  # 输出: 14
```

**重构后:**
```ruby
def sum_of_squares(numbers)
  numbers.map { |number| number**2 }.sum
end

puts sum_of_squares([1, 2, 3])  # 输出: 14
```

重构后的版本使用 Ruby 枚举器更简洁、更清晰地表示相同的逻辑。`map` 方法转换每个元素，`sum` 聚合它们的值，省去了手动循环管理和变量分配的需要。

## 深入了解

重构有着丰富的历史背景，可以追溯到软件开发的早期实践。最初的提及可以追溯到 1990 年代，Martin Fowler 在他的书 "Refactoring: Improving the Design of Existing Code" 中做出了重大贡献，他提供了一个重构模式的目录。从那时起，重构已成为敏捷开发实践的基石。

当我们谈论重构的替代方案时，我们需要考虑不同的方法，如「重写」，在这里你可以部分或完全替代旧系统，或者采用「代码审查」和「结对编程」这样的实践，以逐步改进代码质量。然而，这些并不是重构的替代品；它们是对过程的补充。

在实现方面，Ruby 提供了优秀而富有表现力的语法，经常能在重构后得到更短、更易读的代码。关键原则包括 DRY（不要重复你自己）、使用有意义的名称、保持方法短小并专注于单一任务，以及有效使用 Ruby 的 Enumerable 模块，如上例所示。自动化工具如 RuboCop 也可以帮助程序员识别代码中可能受益于重构的地方。

## 另请参阅

要更深入地了解 Ruby 中的重构，请查看以下资源：

- Martin Fowler 的重要著作：[Refactoring: Improving the Design of Existing Code](https://martinfowler.com/books/refactoring.html)
- Ruby 的编码风格指南，用于编写更干净的代码：[The Ruby Style Guide](https://rubystyle.guide/)
- RuboCop，一个静态代码分析器（linter）和格式化器：[RuboCop GitHub Repository](https://github.com/rubocop/rubocop)
