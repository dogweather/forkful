---
title:                "C#: Delete用于匹配模式的字符。"
programming_language: "C#"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c-sharp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 为什么要删除匹配模式的字符

在编程过程中，经常会遇到需要对字符串进行操作的情况。而有时候，我们可能需要删除一些特定的字符，比如匹配某个特定模式的字符。那么为什么会有这样的需求呢？其中一个可能的原因是为了保证数据的格式统一，或者是为了提取出特定的信息。

## 如何实现删除匹配模式的字符

首先，我们需要确定要删除的字符的匹配模式，例如所有的小写字母或者标点符号。接着，我们可以使用C#中的`Regex`类来实现匹配模式的操作。下面是一个简单的例子：

```C#
using System;
using System.Text.RegularExpressions;

string str = "Hello World!";

// 删除所有的小写字母
string result = Regex.Replace(str, "[a-z]", "");
Console.WriteLine(result); // Output: H W!

// 删除所有的标点符号和空格
string result2 = Regex.Replace(str, "[\\p{P}\\s]", "");
Console.WriteLine(result2); // Output: HelloWorld
```

在上面的例子中，我们使用了两个不同的正则表达式来匹配需要删除的字符。第一个正则表达式使用了字符类`[a-z]`来匹配所有的小写字母，第二个正则表达式使用了Unicode属性上的字符类`\\p{P}`和`\\s`来匹配所有的标点符号和空格。最后，我们使用`Regex.Replace()`方法来替换匹配到的字符为空字符串。

除了使用正则表达式，我们也可以使用LINQ语句来实现删除字符的操作。下面是一个使用LINQ的例子：

```C#
using System;
using System.Linq;

string str = "Hello World!";

// 删除所有的小写字母
string result = new string(str.Where(c => !char.IsLower(c)).ToArray());
Console.WriteLine(result); // Output: H W!

// 删除所有的标点符号和空格
string result2 = new string(str.Where(c => !char.IsSeparator(c) && !char.IsPunctuation(c)).ToArray());
Console.WriteLine(result2); // Output: HelloWorld
```

## 深入了解删除匹配模式的字符

删除匹配模式的字符可以说是一个比较简单的操作，但是在实际应用中可能会遇到一些复杂的情况。比如，当字符串中包含多个匹配模式时，我们可能需要使用不同的方法来处理。另外，正则表达式本身也有一些特殊的语法，需要了解这些语法才能更好地使用正则表达式来匹配字符。

## 参考链接

- [C#正则表达式教程](https://www.runoob.com/csharp/csharp-regular-expressions.html)
- [.NET正则表达式参考指南](https://docs.microsoft.com/zh-cn/dotnet/standard/base-types/regular-expression-language-quick-reference)
- [LINQ入门教程](https://www.runoob.com/linq/linq-tutorial.html)

### 参考链接

- [C#正则表达式教程](https://www.runoob.com/csharp/csharp-regular-expressions.html)
- [.NET正则表达式参考指南](https://docs.microsoft.com/zh-cn/dotnet/standard/base-types/regular-expression-language-quick-reference)
- [LINQ入门教程](https://www.runoob.com/linq/linq-tutorial.html)