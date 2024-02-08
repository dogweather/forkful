---
title:                "从字符串中移除引号"
aliases:
- zh/c-sharp/removing-quotes-from-a-string.md
date:                  2024-01-26T03:38:31.160423-07:00
model:                 gpt-4-0125-preview
simple_title:         "从字符串中移除引号"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c-sharp/removing-quotes-from-a-string.md"
---

{{< edit_this_page >}}

## 何为何故？
在C#中从字符串中移除引号意味着你要去掉包裹你文字的那些讨厌的双引号（`"`）或单引号（`'`）。程序员这么做是为了清理数据、为数据库输入做准备，或者使字符串安全地进行进一步处理，这样当一个偶然的引号出现时，事情就不会变得混乱。

## 如何操作：
```csharp
string withQuotes = "\"Hello, World!\"";
Console.WriteLine($"Original: {withQuotes}");

// 移除双引号
string withoutDoubleQuotes = withQuotes.Replace("\"", "");
Console.WriteLine($"Without Double Quotes: {withoutDoubleQuotes}");

// 假如你的字符串原本就包含单引号，移除单引号
string withSingleQuotes = "'Hello, World!'";
string withoutSingleQuotes = withSingleQuotes.Replace("'", "");
Console.WriteLine($"Without Single Quotes: {withoutSingleQuotes}");
```

输出：
```
Original: "Hello, World!"
Without Double Quotes: Hello, World!
Without Single Quotes: Hello, World!
```

## 深入探讨
移除引号的概念既不新颖也不特别复杂，但它至关重要，因为引号经常被用来界定字符串。当一个带有未转义引号的字符串被包含在代码块或数据文件中时，它可能会过早终止字符串，导致错误或安全问题，如注入攻击。

从历史上看，处理引号一直是数据处理中验证和清理过程的一部分。虽然`.Replace()`方法对于从一个简单的字符串中抽出引号很直接，但你可能需要更先进的技术，如正则表达式，来处理更复杂的场景，比如嵌套引号或有条件的移除。

`.Replace()`的替代方法包括在你需要细粒度控制或处理的是模式而不是固定字符时使用`Regex`类的方法。例如，当处理转义字符时`Regex.Unescape()`可能派上用场。

在实现上，记住C#中的字符串是不可变的，意味着每次使用`.Replace()`时都会创建一个新字符串。对于小型或一次性操作，这没什么大不了的，但对于大型或众多字符串的性能来说，这是需要记在心上的。

## 另请参阅：
- [String.Replace 方法文档](https://docs.microsoft.com/en-us/dotnet/api/system.string.replace?view=netframework-4.8)
- [.NET中的正则表达式](https://docs.microsoft.com/en-us/dotnet/standard/base-types/regular-expressions)
- [安全字符串处理最佳实践](https://www.owasp.org/index.php/Data_Validation)
