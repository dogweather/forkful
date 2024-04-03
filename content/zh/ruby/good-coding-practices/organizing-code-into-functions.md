---
date: 2024-01-26 01:16:03.545883-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u60F3\u8C61\u4F60\u6B63\u5728\u7F16\u5199\
  \u4E00\u4E2A\u5FEB\u901F\u811A\u672C\u6765\u95EE\u5019\u7528\u6237\uFF1A."
lastmod: '2024-03-13T22:44:48.379625-06:00'
model: gpt-4-0125-preview
summary: "\u60F3\u8C61\u4F60\u6B63\u5728\u7F16\u5199\u4E00\u4E2A\u5FEB\u901F\u811A\
  \u672C\u6765\u95EE\u5019\u7528\u6237\uFF1A."
title: "\u5C06\u4EE3\u7801\u7EC4\u7EC7\u6210\u51FD\u6570"
weight: 18
---

## 如何操作：
想象你正在编写一个快速脚本来问候用户：

```Ruby
def greet(name)
  "Hello, #{name}!"
end

puts greet("Alice")   # 输出：Hello, Alice!
puts greet("Bob")     # 输出：Hello, Bob!
```

或者你可能在计算圆的面积：

```Ruby
def circle_area(radius)
  Math::PI * radius ** 2
end

puts circle_area(5)   # 输出：78.53981633974483
```

更整洁、更易于处理，对吧？

## 深入探讨
函数的概念，在Ruby中也称为方法，并不是新事物——它和编程本身一样古老。回溯到1950年代，子程序就被引入，用于减少冗余。

有其他选择吗？当然，你可以使用内联代码，你可以使用面向对象编程，通过类和对象，或者甚至通过lambdas和procs进行函数式编程。但函数是有序代码的基础。想要性能？函数中的局部变量运行快速，函数可以通过 `return` 立即返回值。

在实现上，你可以用 `def` 来定义一个函数，并用 `end` 结束。你可以设置默认参数，对于可变参数函数使用splat操作符等等。根据你的需求，函数可以简单或复杂。

## 另请参阅
- [Ruby的方法文档](https://ruby-doc.org/core-2.7.0/Method.html)
- [通过Chris Pine学习编程](https://pine.fm/LearnToProgram/)
- [Sandi Metz的《Ruby中的实用面向对象设计》](https://www.poodr.com/)
