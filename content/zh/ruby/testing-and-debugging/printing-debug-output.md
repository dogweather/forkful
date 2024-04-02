---
date: 2024-01-20 17:53:38.880197-07:00
description: "\u6253\u5370\u8C03\u8BD5\u8F93\u51FA\u662F\u5728\u4EE3\u7801\u4E2D\u4E34\
  \u65F6\u63D2\u5165\u8BED\u53E5\u4EE5\u67E5\u770B\u53D8\u91CF\u503C\u6216\u7A0B\u5E8F\
  \u6D41\u7A0B\u3002\u7A0B\u5E8F\u5458\u8FD9\u4E48\u505A\u4E3B\u8981\u662F\u4E3A\u4E86\
  \u8DDF\u8E2A\u9519\u8BEF\u6216\u7406\u89E3\u4EE3\u7801\u7684\u6267\u884C\u8DEF\u5F84\
  \u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:48.376037-06:00'
model: gpt-4-1106-preview
summary: "\u6253\u5370\u8C03\u8BD5\u8F93\u51FA\u662F\u5728\u4EE3\u7801\u4E2D\u4E34\
  \u65F6\u63D2\u5165\u8BED\u53E5\u4EE5\u67E5\u770B\u53D8\u91CF\u503C\u6216\u7A0B\u5E8F\
  \u6D41\u7A0B\u3002\u7A0B\u5E8F\u5458\u8FD9\u4E48\u505A\u4E3B\u8981\u662F\u4E3A\u4E86\
  \u8DDF\u8E2A\u9519\u8BEF\u6216\u7406\u89E3\u4EE3\u7801\u7684\u6267\u884C\u8DEF\u5F84\
  \u3002"
title: "\u6253\u5370\u8C03\u8BD5\u8F93\u51FA"
weight: 33
---

## What & Why? (是什么？为什么？)
打印调试输出是在代码中临时插入语句以查看变量值或程序流程。程序员这么做主要是为了跟踪错误或理解代码的执行路径。

## How to: (怎么做？)
在Ruby中，你可以使用 `puts` 或 `p` 来打印调试信息。

```Ruby
def calculate_sum(a, b)
  puts "Adding #{a} and #{b}"  # 输出调试信息
  a + b
end

result = calculate_sum(5, 3)
puts "The result is #{result}"  # 再次打印结果
```

输出结果：
```
Adding 5 and 3
The result is 8
```

## Deep Dive (深入了解)
早期编程时，`print` 语句是最基本的调试工具。Ruby继承了这个概念，提供 `puts` 和 `p` 方法，两者有细微差别。`puts` 相对较人性化，会自动添加换行符，适用于人类阅读的输出。而 `p` 则输出更多原始信息，适用于详细调试数据结构。另外，你也可以使用 `pp` 来漂亮地打印复杂数据结构。

除了打印输出，还有其他调试工具，比如使用 Ruby 的标准库 `debugger` 或者 `byebug` gem 来逐行执行代码。

## See Also (另请参阅)
- [Ruby官方文档关于IO](https://ruby-doc.org/core-3.0.0/IO.html)
- [Ruby-Doc.org的Kernel#p](https://ruby-doc.org/core-3.0.0/Kernel.html#method-i-p)
- [Byebug gem on GitHub](https://github.com/deivid-rodriguez/byebug)
- [‘puts’ vs ‘p’ vs ‘print’ in Ruby on Stack Overflow](https://stackoverflow.com/questions/1255324/puts-vs-print-in-ruby)
