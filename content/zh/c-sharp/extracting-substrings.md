---
title:                "提取子字符串"
html_title:           "Arduino: 提取子字符串"
simple_title:         "提取子字符串"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c-sharp/extracting-substrings.md"
---

{{< edit_this_page >}}

## 什么 & 为什么?

字符串的提取（Extracting substrings）是获取字符串部分内容的过程。它对分析和处理数据，如日志文件或用户输入，非常有用。

## 如何操作:

使用C#中的 `Substring` 方法，您可以轻松地提取子字符串。看以下例子:

```C#
string s = "Hello, World!";
string sub = s.Substring(7, 5);
Console.WriteLine(sub);
```

在以上的例子中，`Substring` 方法从位置 7 开始的位置，取到接下来的5个字符（注意：我们从 0 开始计算位置）。所以输出将是 "World"。

## 深入探讨:

在 C# 的早期版本中，`String.Substring` 是提取子字符串的主要方法。然而，在一些情况下，它可能会导致内存使用效率低下。在最新版本的 C#, 微软引入了 `String.AsSpan` 和 `MemoryExtensions.AsSpan` 方法作为 `Substring` 的替代，可以更有效地处理内存。

例如，以下代码与之前示例的 `Substring` 方法做同样的操作，但不创建新的字符串:
```C#
string s = "Hello, World!";
ReadOnlySpan<char> span = s.AsSpan(7, 5);
foreach(var ch in span)
{
   Console.Write(ch);
}
```
此代码输出也是 "World"，但此处并未创建新的字符串。

## 另见:

* [Microsoft的官方文档：String.Substring Method](https://docs.microsoft.com/en-us/dotnet/api/system.string.substring?view=net-5.0)
* [Microsoft的官方文档：MemoryExtensions.AsSpan Method](https://docs.microsoft.com/en-us/dotnet/api/system.memoryextensions.asspan?view=net-5.0)