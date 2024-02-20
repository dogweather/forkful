---
date: 2024-01-26 01:16:03.545883-07:00
description: "\u5C06\u4EE3\u7801\u7EC4\u7EC7\u6210\u51FD\u6570\u53EF\u4EE5\u5C06\u811A\
  \u672C\u5206\u89E3\u4E3A\u53EF\u91CD\u590D\u4F7F\u7528\u7684\u5757\u3002\u8FD9\u4E00\
  \u5207\u90FD\u662F\u4E3A\u4E86\u4F7F\u4EE3\u7801\u66F4\u52A0\u6E05\u6670\u3001\u6613\
  \u4E8E\u7BA1\u7406\u3001\u5E76\u51CF\u5C11\u9519\u8BEF\u3002\u6A21\u5757\u5316\u7684\
  \u4EE3\u7801\u4E4B\u6240\u4EE5\u4F18\u79C0\uFF0C\u662F\u56E0\u4E3A\u5B83\u53EF\u4EE5\
  \u8282\u7701\u65F6\u95F4\u3001\u7EF4\u62A4\u4F60\u7684\u7406\u667A\uFF0C\u5E76\u7B80\
  \u5316\u8C03\u8BD5\u548C\u5355\u5143\u6D4B\u8BD5\u3002"
lastmod: 2024-02-19 22:05:07.441759
model: gpt-4-0125-preview
summary: "\u5C06\u4EE3\u7801\u7EC4\u7EC7\u6210\u51FD\u6570\u53EF\u4EE5\u5C06\u811A\
  \u672C\u5206\u89E3\u4E3A\u53EF\u91CD\u590D\u4F7F\u7528\u7684\u5757\u3002\u8FD9\u4E00\
  \u5207\u90FD\u662F\u4E3A\u4E86\u4F7F\u4EE3\u7801\u66F4\u52A0\u6E05\u6670\u3001\u6613\
  \u4E8E\u7BA1\u7406\u3001\u5E76\u51CF\u5C11\u9519\u8BEF\u3002\u6A21\u5757\u5316\u7684\
  \u4EE3\u7801\u4E4B\u6240\u4EE5\u4F18\u79C0\uFF0C\u662F\u56E0\u4E3A\u5B83\u53EF\u4EE5\
  \u8282\u7701\u65F6\u95F4\u3001\u7EF4\u62A4\u4F60\u7684\u7406\u667A\uFF0C\u5E76\u7B80\
  \u5316\u8C03\u8BD5\u548C\u5355\u5143\u6D4B\u8BD5\u3002"
title: "\u5C06\u4EE3\u7801\u7EC4\u7EC7\u6210\u51FD\u6570"
---

{{< edit_this_page >}}

## 什么 & 为什么？
将代码组织成函数可以将脚本分解为可重复使用的块。这一切都是为了使代码更加清晰、易于管理、并减少错误。模块化的代码之所以优秀，是因为它可以节省时间、维护你的理智，并简化调试和单元测试。

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
