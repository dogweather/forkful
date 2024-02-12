---
title:                "将代码组织成函数"
aliases:
- /zh/ruby/organizing-code-into-functions.md
date:                  2024-01-26T01:16:03.545883-07:00
model:                 gpt-4-0125-preview
simple_title:         "将代码组织成函数"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/ruby/organizing-code-into-functions.md"
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
