---
title:    "C#: 计算字符串的长度"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/c-sharp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 为什么

在编写C#程序时，有时候我们会遇到需要知道字符串长度的情况。比如，我们可能需要验证用户输入的密码是否符合要求，或者需要截取字符串的一部分。在这些情况下，找到字符串的长度就变得非常重要。

## 如何做

要找到字符串的长度，我们可以使用C#内置的`Length`属性。例如，假设我们有一个名为`str`的字符串变量，我们可以使用以下代码来找到它的长度：

```C#
int length = str.Length;
```

我们也可以在控制台输出字符串的长度，让我们来看一个完整的例子：

```C#
string str = "你好世界";
int length = str.Length;
Console.WriteLine("字符串 “{0}” 的长度为 {1}。", str, length);
```

输出会是：

```
字符串 “你好世界” 的长度为 4。
```

## 深入探讨

在C#中，字符串是一个类，而不是基本数据类型。这意味着字符串变量实际上是一个指向字符串对象的引用。而该字符串对象包含一个`Length`属性，用于存储字符串的实际长度。

另外值得注意的是，`Length`属性返回的值是一个`int`类型。这意味着，如果我们想要获取一个字符串的最大长度，我们需要确保返回的值不会超过`int`类型的最大值。

## 参考资料

- [String.Length Property](https://docs.microsoft.com/en-us/dotnet/api/system.string.length?view=net-5.0)
- [C#字符串操作指南](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/strings/)