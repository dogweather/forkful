---
date: 2024-01-26 03:36:38.019447-07:00
description: "\u5982\u4F55\u8FDB\u884C\uFF1A \u8BA9\u6211\u4EEC\u901A\u8FC7\u91CD\u6784\
  \u4E00\u4E2A\u8BA1\u7B97\u5E73\u65B9\u548C\u7684 Ruby \u65B9\u6CD5\u7684\u4F8B\u5B50\
  \u6765\u8BF4\u660E\u3002 **\u91CD\u6784\u524D:**."
lastmod: '2024-04-05T21:53:48.656779-06:00'
model: gpt-4-0125-preview
summary: ''
title: "\u91CD\u6784"
weight: 19
---

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
