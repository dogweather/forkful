---
title:                "将字符串转化为大写"
html_title:           "C#: 将字符串转化为大写"
simple_title:         "将字符串转化为大写"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c-sharp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？
将字符串的首字母大写就是将字符串中的每个单词的首字母变为大写字母。程序员之所以要这样做，是因为它可以方便我们在阅读代码时更加清晰，同时也符合编程的一些规范。

## 如何操作：
我们可以使用C#中的 `TextInfo.ToTitleCase` 方法来实现首字母大写。下面是一段示例代码：
```C#
using System;
using System.Globalization;
 
public class Example
{
    public static void Main()
    {
        TextInfo myTI = new CultureInfo("en-US",false).TextInfo;
        string myString = "hello world";

        Console.WriteLine($"原文：{myString}");
        Console.WriteLine($"修改后: {myTI.ToTitleCase(myString)}");
    }
}
```
当你运行这段程序，你会得到以下输出：
```C#
原文：hello world
修改后: Hello World
```
这个代码简洁明了，它构建了一个字符串 "hello world"，然后使用 `TextInfo.ToTitleCase`方法改变了每个单词的首字母为大写字母。

## 深入了解
首字母大写在编程历史上就有其特定的作用和意义。在C#之前的语言中，如C和C++，我们通常需要手动编写函数去实现首字母大写。随着.NET框架的出现，我们现在可以使用内建的`TextInfo.ToTitleCase`方法来进行操作。

当然，我们也可以使用其他方式来实现首字母大写。例如，我们可以使用 `ToUpper` 方法加上字符串拆分和拼接操作，但这种方式相比使用 `TextInfo.ToTitleCase`更为复杂。

`TextInfo.ToTitleCase` 控制首字母大写是通过对某些特定的Unicode类别生效。如果您对这方面感兴趣，可以查看`.NET`的官方文档。

## 见所未见
关于C#字符串更深入的资料，可以参考以下链接：
- Microsoft官方C#编程指南 [C# programming guide](https://docs.microsoft.com/en-us/dotnet/csharp/)
- 关于.NET的 `TextInfo`类的 [详细文档](https://docs.microsoft.com/zh-cn/dotnet/api/system.globalization.textinfo?view=netframework-4.7.2)
- 更深入了解字符串处理的 [字符串处理和正则表达式](https://docs.microsoft.com/zh-cn/dotnet/standard/base-types/)