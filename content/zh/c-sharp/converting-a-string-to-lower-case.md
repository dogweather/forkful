---
title:                "将字符串转换为小写"
date:                  2024-01-20T17:38:04.467472-07:00
model:                 gpt-4-1106-preview
simple_title:         "将字符串转换为小写"

category:             "C#"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c-sharp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why? (什么以及为什么?)
把字符串转换成小写意味着将所有大写字符改为小写格式。程序员这么做以实现数据一致性、比较操作，以及满足大小写不敏感的搜索需求。

## How to: (如何操作)
```C#
string originalText = "Hello, World!";
string lowerCaseText = originalText.ToLower();

Console.WriteLine(lowerCaseText);  // 输出: hello, world!
```
简单的`ToLower()`方法就可以把字符串转换成全小写。

## Deep Dive (深入了解)
在.NET中，转换字符串到小写已被嵌入多年，一个重要的方法是`ToLower()`。不同地区的文本有不同的大小写规则，因此.NET还提供了`ToLowerInvariant()`，该方法不考虑区域性地来转换。还有一些替代方法，如LINQ：

```C#
string mixedCase = "C# Programming";
string lower = new string(mixedCase.Select(char.ToLower).ToArray());

Console.WriteLine(lower);  // 输出: c# programming
```

但是`ToLower()`方法因其简便性而被广泛使用。在将字符串转向小写时要注意，一些文化区域对字母的大小写转换有特殊规则，可能不符合你的预期。所以在全球化应用程序中需要特别注意这点。

## See Also (另请参阅)
- Microsoft Docs - String.ToLower: https://docs.microsoft.com/en-us/dotnet/api/system.string.tolower
- Microsoft Docs - String.ToLowerInvariant: https://docs.microsoft.com/en-us/dotnet/api/system.string.tolowerinvariant
- Stack Overflow - Case Insensitive String Operations: https://stackoverflow.com/questions/444798/case-insensitive-string-comparison-in-c-sharp
