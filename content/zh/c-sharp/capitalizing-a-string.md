---
title:                "字符串首字母大写"
html_title:           "Arduino: 字符串首字母大写"
simple_title:         "字符串首字母大写"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c-sharp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (是什么？为什么要做？)
字符串大写，就是将字符串中的字母转换成大写格式。程序员这么做可能是为了格式统一，也可能是处理编程中的文本数据。

## How to: (如何操作：)
在C#中，使用`.ToUpper()`方法可以轻松实现字符串大写转换。

```C#
string originalText = "hello, world!";
string uppercasedText = originalText.ToUpper();

Console.WriteLine(uppercasedText);  // 输出：HELLO, WORLD!
```

## Deep Dive (深入了解)
早期的编程语言处理字符串时，并没有提供现成的大写转换方法。开发者需要手动实现逐字符的大小写转换。`.ToUpper()`方法提供了一个简单、高效的方式来转换整个字符串。除了`.ToUpper()`，C#还有`.ToUpperInvariant()`，它在进行大写转换时忽略了特定文化的大小写规则，保证了结果的一致性。在内部，这些方法通过字符映射表来实现转换。

## See Also (参见)
- MSDN 文档上的 `.ToUpper()` 方法说明：[https://docs.microsoft.com/en-us/dotnet/api/system.string.toupper](https://docs.microsoft.com/en-us/dotnet/api/system.string.toupper)
- 字符串操作最佳实践：[https://docs.microsoft.com/en-us/dotnet/standard/base-types/best-practices-strings](https://docs.microsoft.com/en-us/dotnet/standard/base-types/best-practices-strings)
- 关于编程文化差异和`.ToUpperInvariant()`方法的讨论：[https://docs.microsoft.com/en-us/dotnet/api/system.string.toupperinvariant](https://docs.microsoft.com/en-us/dotnet/api/system.string.toupperinvariant)
