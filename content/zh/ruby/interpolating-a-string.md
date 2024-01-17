---
title:                "插入、調整字串"
html_title:           "Ruby: 插入、調整字串"
simple_title:         "插入、調整字串"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/ruby/interpolating-a-string.md"
---

{{< edit_this_page >}}

# 什么是字符串插值？为什么程序员要这样做？

字符串插值是在字符串中嵌入变量值或表达式的方法。程序员经常使用它来创建动态的文本，例如在打印日志或创建输出消息时。它可以让我们在字符串中轻松地组合变量和文本，使代码更加精简和易读。

## 如何操作：

```Ruby
# 这是一个字符串插值的简单例子
name = "Tina"
age = 30
puts "欢迎来到我的个人主页！我叫#{name}，今年#{age}岁。"
# 输出结果：欢迎来到我的个人主页！我叫Tina，今年30岁。
```

```Ruby
# 我们也可以在插值表达式中使用方法
num1 = 5
num2 = 10
puts "我最近学会了一些数学技巧，可以计算出#{num1}与#{num2}的和，结果是#{num1 + num2}。"
# 输出结果：我最近学会了一些数学技巧，可以计算出5与10的和，结果是15。
```

## 深入探讨：

字符串插值最早出现在1970年的Unix操作系统中，但直到20世纪80年代和90年代才变得流行起来。替代方案包括使用“连接”和“格式化”方法来构建字符串，但插值更加简洁和直观。Ruby的插值实现原理是将插值表达式转换为Ruby代码，然后将其插入到字符串中。

## 参考资料：

- [Ruby官方文档：字符串插值](https://ruby-doc.org/core-3.0.0/doc/syntax/literals_rdoc.html#label-String+Interpolation)
- [博客文章：为什么要使用字符串插值？](https://www.honeybadger.io/blog/using-string-interpolation-in-ruby/)
- [知乎讨论：为什么插值比连接和格式化更受欢迎?](https://www.zhihu.com/question/19919801)