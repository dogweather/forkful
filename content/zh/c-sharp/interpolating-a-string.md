---
title:                "插值字符串"
html_title:           "Arduino: 插值字符串"
simple_title:         "插值字符串"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c-sharp/interpolating-a-string.md"
---

{{< edit_this_page >}}

## 什么以及为什么?
字符串插值是一种在 C# 中创建字符串的方式，通过在大括号 `{} `中插入变量或表达式来生成新字符串。程序员之所以使用字符串插值，主要是为了提高代码的可读性和便捷性。

## 如何实现:
让我们来看一个简单的例子:
```C#
string name = "Michael";
float score = 85.6f;
string result = $"学生{name}的成绩是{score}";
Console.WriteLine(result);
```
这会输出: `学生Michael的成绩是85.6`

## 深入研究:
字符串插值在 C# 6.0 时引入，作为对传统的 `String.Format` 方法的补充。除了字符串插值，你还可以使用 `+` 运算符或 `StringBuilder` 以创建复杂的字符串，但使用字符串插值可能更简洁易懂。字符串插值在运行时被转化为 `String.Format` 方法，因此性能影响微乎其微。

## 参看以下资料:
- 官方C# 文档 [String interpolation (C# reference)](https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/tokens/interpolated)
- MSDN [String Interpolation in C#](https://msdn.microsoft.com/en-us/magazine/mt614271.aspx)
- StackOverflow上关于[String Interpolation Efficiency](https://stackoverflow.com/questions/36124656/is-string-interpolation-in-c-sharp-efficient)的讨论.