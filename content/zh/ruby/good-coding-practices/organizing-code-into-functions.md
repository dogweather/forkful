---
date: 2024-01-26 01:16:03.545883-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u51FD\u6570\u7684\u6982\u5FF5\uFF0C\u5728\
  Ruby\u4E2D\u4E5F\u79F0\u4E3A\u65B9\u6CD5\uFF0C\u5E76\u4E0D\u662F\u65B0\u4E8B\u7269\
  \u2014\u2014\u5B83\u548C\u7F16\u7A0B\u672C\u8EAB\u4E00\u6837\u53E4\u8001\u3002\u56DE\
  \u6EAF\u52301950\u5E74\u4EE3\uFF0C\u5B50\u7A0B\u5E8F\u5C31\u88AB\u5F15\u5165\uFF0C\
  \u7528\u4E8E\u51CF\u5C11\u5197\u4F59\u3002\u2026"
lastmod: '2024-04-05T22:51:01.578319-06:00'
model: gpt-4-0125-preview
summary: "\u6709\u5176\u4ED6\u9009\u62E9\u5417\uFF1F\u5F53\u7136\uFF0C\u4F60\u53EF\
  \u4EE5\u4F7F\u7528\u5185\u8054\u4EE3\u7801\uFF0C\u4F60\u53EF\u4EE5\u4F7F\u7528\u9762\
  \u5411\u5BF9\u8C61\u7F16\u7A0B\uFF0C\u901A\u8FC7\u7C7B\u548C\u5BF9\u8C61\uFF0C\u6216\
  \u8005\u751A\u81F3\u901A\u8FC7lambdas\u548Cprocs\u8FDB\u884C\u51FD\u6570\u5F0F\u7F16\
  \u7A0B\u3002\u4F46\u51FD\u6570\u662F\u6709\u5E8F\u4EE3\u7801\u7684\u57FA\u7840\u3002\
  \u60F3\u8981\u6027\u80FD\uFF1F\u51FD\u6570\u4E2D\u7684\u5C40\u90E8\u53D8\u91CF\u8FD0\
  \u884C\u5FEB\u901F\uFF0C\u51FD\u6570\u53EF\u4EE5\u901A\u8FC7 `return` \u7ACB\u5373\
  \u8FD4\u56DE\u503C\u3002"
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
