---
title:                "C#: 计算字符串的长度"
programming_language: "C#"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c-sharp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# 为什么：为什么要找出字符串的长度？

字符串是编程中经常用到的数据类型，它可以存储文本信息。但是，有时候我们需要知道一个字符串有多长，比如在做表单验证或者处理文本时。因此，找出字符串的长度可以帮助我们更好地处理文本信息。

## 如何做：使用C#找出字符串的长度

首先，我们需要声明一个字符串变量并赋值。然后，使用C#内置的Length属性来获取字符串的长度。最后，将长度值打印出来。

```C#
string myString = "Hello World!";
int length = myString.Length;
Console.WriteLine("字符串的长度为：" + length);
```

输出结果：
`字符串的长度为：12`

我们也可以使用for循环来逐个遍历字符串中的字符，并使用计数器来统计字符的个数。最后，将计数器的值作为字符串的长度。

```C#
string myString = "Hello World!";
int counter = 0;
for (int i = 0; i < myString.Length; i++)
{
    counter++;
}
Console.WriteLine("字符串的长度为：" + counter);
```

输出结果：
`字符串的长度为：12`

## 深入了解

在C#中，字符串是一个字符序列，每个字符都有一个索引值。字符串的长度就是字符串中字符的个数。当我们使用Length属性时，实际上是在获取字符串中字符的个数，这也解释了为什么字符串的长度和索引值是不同的概念。

此外，C#中的字符串是不可变的，这意味着每次对字符串的修改都会创建一个新的字符串。因此，获取字符串的长度可以帮助我们更好地处理字符串，避免不必要的字符串拼接和修改操作。

# 同时请看

- [C#字符串操作指南](https://blog.csdn.net/qq_38327164/article/details/83392333)
- [C#变量和数据类型](https://blog.csdn.net/m0_38106923/article/details/78043734)