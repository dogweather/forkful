---
title:                "插值字符串"
html_title:           "C#: 插值字符串"
simple_title:         "插值字符串"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c-sharp/interpolating-a-string.md"
---

{{< edit_this_page >}}

什么是字符串内插? 为什么程序员要这么做？

字符串内插是一种编程技术，它允许我们在一个字符串中插入变量或表达式。它的作用是让我们能够动态地构建字符串，从而使我们的代码更简洁、更易读。程序员使用字符串内插来构建动态信息，比如打印日志消息或创建用户界面。

如何实现字符串内插:

```C#
string name = "小明";
int age = 23;
Console.WriteLine($"我是{name}, 我的年龄是{age}岁。"); // 输出: 我是小明, 我的年龄是23岁。
```

深入介绍:

字符串内插最早出现在Python编程语言中，被称为“格式化字符串”。它简化了字符串拼接的步骤，使代码更可读。虽然在C#中，我们也可以使用字符串拼接来构建动态信息，但是字符串内插更加优雅和方便。另一个替代方案是使用String.Format()方法，它也能实现类似的功能，但是语法上稍显复杂。

代码示例:

```C#
string name = "小红";
int age = 27;
Console.WriteLine(String.Format("我是{0}, 我的年龄是{1}岁。", name, age)); // 输出: 我是小红, 我的年龄是27岁。
```

相关链接:

- [官方文档](https://docs.microsoft.com/zh-cn/dotnet/csharp/language-reference/tokens/interpolated)
- [Python官方文档](https://docs.python.org/3/tutorial/inputoutput.html#formatted-string-literals)