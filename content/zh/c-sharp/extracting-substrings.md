---
title:    "C#: 提取子字符串"
keywords: ["C#"]
---

{{< edit_this_page >}}

## 为什么

在编程中，有时候我们需要从一个字符串中提取出一部分内容，而不是使用整个字符串。这种操作称为“提取子字符串”。它可以帮助我们更有效地处理字符串数据，节省时间和精力。

## 如何做

要提取子字符串，我们可以使用C#语言提供的SubString()方法。这个方法需要两个参数，分别是开始和结束位置的索引。让我们来看一个实际的例子：

```C#
string str = "这是一个字符串。";

string subStr = str.Substring(4, 4);

Console.WriteLine(subStr);

// Output: 一个字符
```

在这个例子中，我们从索引为4的位置开始提取，一直提取4个字符，得到的结果就是“一个字符”。

## 深入了解

除了提供起始和结束位置的索引，SubString()方法还有一个重载的版本，它可以只提供起始位置的索引，然后会一直提取到字符串的末尾。另外，这个方法也可以使用负数作为参数，它会从字符串的末尾开始计算索引。例如：

```C#
string str = "这是一个字符串。";

string subStr = str.Substring(4);

Console.WriteLine(subStr);

// Output: 一个字符串。
```

在这个例子中，我们只指定了起始位置的索引，因此会一直提取到字符串的末尾。我们还可以使用负数来提取从字符串末尾往前数的位置，例如：

```C#
string str = "这是一个字符串。";

string subStr = str.Substring(-4);

Console.WriteLine(subStr);

// Output: 字符串。
```

这个例子中，我们使用负数-4来提取从“字符”开始的部分，得到的结果就是“字符”。

## 参考链接

- [C#字符串提取子串文档](https://docs.microsoft.com/zh-cn/dotnet/api/system.string.substring)
- [详解SubString()方法](https://www.cnblogs.com/bluexx/archive/2011/10/28/2250621.html)