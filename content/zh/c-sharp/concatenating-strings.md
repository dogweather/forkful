---
title:                "C#: 连接字符串"
simple_title:         "连接字符串"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c-sharp/concatenating-strings.md"
---

{{< edit_this_page >}}

## 为什么需要拼接字符串？

在C#编程中，拼接字符串是常见且必不可少的操作。当我们需要将多个字符串连接为一个完整的字符串时，拼接字符串就会派上用场。例如，在编写网页时，我们可能需要将用户输入的不同文本片段拼接为一段完整的HTML代码。同时，在处理文本数据时，拼接字符串也是不可或缺的步骤。拼接字符串的作用在于简化代码结构，使得处理字符串变得更加灵活。

## 如何进行字符串拼接？

在C#中，字符串拼接可以通过"+"运算符实现。例如，我们要拼接两个姓名"张三"和"李四"，可以使用下面的代码：

```C#
string firstName = "张三";
string lastName = "李四";
string fullName = firstName + lastName;
Console.WriteLine(fullName);
```

运行以上代码将输出"张三李四"。此外，我们还可以使用字符串插值来进行拼接，这种方法更加简洁直观。例如，我们可以将上述代码改写为：

```C#
string firstName = "张三";
string lastName = "李四";
string fullName = $"{firstName}{lastName}";
Console.WriteLine(fullName);
```

同样，运行结果也是"张三李四"。除了使用字符串插值外，我们还可以使用String类中的Concat方法进行拼接，该方法可以接受多个字符串作为参数。例如：

```C#
string firstName = "张三";
string lastName = "李四";
string fullName = String.Concat(firstName, lastName);
Console.WriteLine(fullName);
```

## 更深层次的拼接字符串

在C#中，字符串是不可变的，这意味着每次对字符串的修改都会创建新的内存实例。因此，在涉及大量字符串拼接的情况下，我们应该尽可能避免使用"+"运算符。相反，可以使用String类中的StringBuilder来提高性能。StringBuilder是可变的字符串，当需要对字符串进行多次操作时，使用它会更加高效。以下是使用StringBuilder进行字符串拼接的示例：

```C#
StringBuilder sb = new StringBuilder();
sb.Append("张三");
sb.Append("李四");
string fullName = sb.ToString();
Console.WriteLine(fullName);
```
除了上面介绍的方法外，C#还提供了多种字符串拼接的方式，例如使用格式化字符串和正则表达式等。通过学习不同的方法，我们可以根据实际情况选择最合适的方式来进行字符串拼接。

## 查看也可以

- [C#官方文档-字符串](https://docs.microsoft.com/zh-cn/dotnet/csharp/programming-guide/strings/)
- [C#教程-字符串拼接](http://www.w3school.com.cn/csharp/csharp_ref_string_concat.asp)
- [字符串拼接的性能比较](https://stackoverflow.com/questions/21766434/string-concatenation-vs-stringbuilder-which-is-more-efficient)