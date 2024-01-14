---
title:                "C#: 将字符串转换为小写"
programming_language: "C#"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c-sharp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

为什么：字符串转换为小写的动机是什么？

## Why

字符串大小写在编程中有着重要的作用。例如，当我们需要比较两个字符串时，它们的大小写必须相同。因此，将一个字符串转换为小写是一个很常见的需求，这样就可以方便地进行比较。另外，有些编程语言的语法规则也要求变量名必须是小写，因此需要将字符串转换为小写。

## How To

在C#中，我们可以使用ToLower()方法来将字符串转换为小写。例如，我们有一个名为"Hello World"的字符串，我们想要将它转换为小写并输出到控制台上，可以按照以下步骤实现：

```C#
string str = "Hello World";
string lowerCaseStr = str.ToLower();
Console.WriteLine(lowerCaseStr);
```

输出结果为："hello world"

我们也可以在将转换后的字符串赋值给另一个变量的同时，将其打印出来：

```C#
string str = "Hello World";
Console.WriteLine(str.ToLower());
```

输出结果同样为："hello world"

## Deep Dive

在C#中，将字符串转换为小写的实现原理是通过调用字符串的ToLower()方法，然后返回一个新的字符串对象。这个过程中，所有大写字母都将被转换为对应的小写字母，并复制到新的字符串中。同时，原始字符串不会被修改。

另外，ToLower()方法也有一个重载版本，它可以接受一个CultureInfo对象作为参数。这个参数可以用来指定字符串应该按照哪种语言的规则进行大小写转换。默认情况下，会使用当前系统的默认语言规则。

See Also（查看更多相关信息）：

- C#字符串操作文档：https://docs.microsoft.com/zh-cn/dotnet/csharp/programming-guide/strings/
- 了解CultureInfo对象：https://docs.microsoft.com/zh-cn/dotnet/api/system.globalization.cultureinfo?view=net-5.0