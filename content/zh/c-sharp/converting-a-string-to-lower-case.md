---
title:                "转换字符串为小写"
html_title:           "C#: 转换字符串为小写"
simple_title:         "转换字符串为小写"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c-sharp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 为什么？（Why）

为了使字符串具有统一的格式，有时需要将其转换为小写。这样可以确保在字符串比较和搜索时不会因为大小写不同而出现错误。

## 如何？（How To)

使用 C# 中的内置方法 `ToLower()` 可以将字符串转换为小写形式。在下面的示例中，我们将使用两种不同的方法来演示这一过程：

```C#
string str = "Hello World";
Console.WriteLine(str.ToLower()); // 输出 "hello world"

// 使用 for 循环手动将每个字符转换为小写形式
for (int i = 0; i < str.Length; i++)
{
    Console.Write(Char.ToLower(str[i])); // 输出 "hello world"
}
```

## 深入了解（Deep Dive)

字符串是不可变类型，这意味着原始字符串本身不会改变，而是返回一个新的字符串。因此，将字符串转换为小写形式会创建一个新的字符串对象。

此外，`ToLower()` 方法默认会使用当前系统的文化信息来确定如何进行大小写转换。但如果需要，可以通过使用重载的 `ToLower()` 方法来指定特定的文化信息。

## 更多参考（See Also)

- [Microsoft 文档：ToLower() 方法](https://docs.microsoft.com/en-us/dotnet/api/system.string.tolower)
- [C# 字符串操作指南](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/strings/)