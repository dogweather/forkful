---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:05:18.350648-07:00
description: "\u5982\u4F55\u505A\uFF1A C#\u63D0\u4F9B\u4E86\u4E00\u4E2A\u7B80\u5355\
  \u7684\u65B9\u6CD5\u6765\u901A\u8FC7\u5185\u7F6E\u65B9\u6CD5\u5927\u5199\u5B57\u7B26\
  \u4E32\u3002\u5B9E\u73B0\u8FD9\u4E00\u70B9\u7684\u6700\u7B80\u5355\u65B9\u5F0F\u662F\
  \u76F4\u63A5\u7528\u8FD9\u4E9B\u65B9\u6CD5\u4FEE\u6539\u5B57\u7B26\u4E32\u3002\u5BF9\
  \u4E8E\u66F4\u590D\u6742\u6216\u7279\u5B9A\u7684\u5927\u5199\u89C4\u5219\uFF08\u4F8B\
  \u5982\uFF0C\u6BCF\u4E2A\u5355\u8BCD\u7684\u9996\u5B57\u6BCD\u5927\u5199\uFF09\uFF0C\
  \u53EF\u80FD\u9700\u8981\u989D\u5916\u7684\u5E93\u6216\u624B\u52A8\u65B9\u6CD5\u3002\
  \u4EE5\u4E0B\u662F\u5728C#\u4E2D\u4EE5\u5404\u79CD\u65B9\u5F0F\u5927\u5199\u5B57\
  \u7B26\u4E32\u7684\u793A\u4F8B\u3002"
lastmod: '2024-04-05T22:38:46.911331-06:00'
model: gpt-4-0125-preview
summary: "\u5982\u4F55\u505A\uFF1A C#\u63D0\u4F9B\u4E86\u4E00\u4E2A\u7B80\u5355\u7684\
  \u65B9\u6CD5\u6765\u901A\u8FC7\u5185\u7F6E\u65B9\u6CD5\u5927\u5199\u5B57\u7B26\u4E32\
  \u3002\u5B9E\u73B0\u8FD9\u4E00\u70B9\u7684\u6700\u7B80\u5355\u65B9\u5F0F\u662F\u76F4\
  \u63A5\u7528\u8FD9\u4E9B\u65B9\u6CD5\u4FEE\u6539\u5B57\u7B26\u4E32\u3002\u5BF9\u4E8E\
  \u66F4\u590D\u6742\u6216\u7279\u5B9A\u7684\u5927\u5199\u89C4\u5219\uFF08\u4F8B\u5982\
  \uFF0C\u6BCF\u4E2A\u5355\u8BCD\u7684\u9996\u5B57\u6BCD\u5927\u5199\uFF09\uFF0C\u53EF\
  \u80FD\u9700\u8981\u989D\u5916\u7684\u5E93\u6216\u624B\u52A8\u65B9\u6CD5\u3002\u4EE5\
  \u4E0B\u662F\u5728C#\u4E2D\u4EE5\u5404\u79CD\u65B9\u5F0F\u5927\u5199\u5B57\u7B26\
  \u4E32\u7684\u793A\u4F8B\u3002"
title: "\u5B57\u7B26\u4E32\u5927\u5199\u5316"
weight: 2
---

## 如何做：
C#提供了一个简单的方法来通过内置方法大写字符串。实现这一点的最简单方式是直接用这些方法修改字符串。对于更复杂或特定的大写规则（例如，每个单词的首字母大写），可能需要额外的库或手动方法。以下是在C#中以各种方式大写字符串的示例。

### 基本大写：
要大写单个单词或句子的第一个字母：

```csharp
string originalString = "hello world";
string capitalizedString = char.ToUpper(originalString[0]) + originalString.Substring(1);
Console.WriteLine(capitalizedString); // 输出："Hello world"
```

### 每个单词的首字母大写：
要大写字符串中每个单词的首字母，你可以使用在`System.Globalization`命名空间中找到的`TextInfo.ToTitleCase`方法：

```csharp
using System;
using System.Globalization;

string originalString = "hello world";
TextInfo textInfo = CultureInfo.CurrentCulture.TextInfo;
string capitalizedString = textInfo.ToTitleCase(originalString);
Console.WriteLine(capitalizedString); // 输出："Hello World"
```

注意：`ToTitleCase`不会将其余字母的大小写转为小写；它只改变每个单词的第一个字母为大写。此外，根据文化设置，某些单词在标题大小写规则中（如 "and", "or", "of"）可能不会被大写。

### 使用扩展方法以提高可重用性：
你可以为`string`类创建一个扩展方法以简化大写过程，使你的代码更清洁且更具可重用性。以下是创建和使用这样一个方法的方式：

```csharp
using System;

public static class StringExtensions
{
    public static string Capitalize(this string input)
    {
        if (string.IsNullOrEmpty(input))
        {
            return input;
        }
        return char.ToUpper(input[0]) + input.Substring(1);
    }
}

class Program
{
    static void Main(string[] args)
    {
        string originalString = "hello world";
        string capitalizedString = originalString.Capitalize();
        Console.WriteLine(capitalizedString); // 输出："Hello world"
    }
}
```

这个扩展方法`Capitalize`可以在命名空间内的任何字符串对象上调用，提供一种更直观且面向对象的方法来在C#中操作字符串。

### 第三方库：
虽然C#的标准库覆盖了字符串大写需求的大部分情况，但某些特殊任务可能会从第三方库（如Humanizer）中受益。然而，对于仅仅大写字符串或字符串中的每个单词的任务，标准C#方法就足够有效，不需要外部依赖。
