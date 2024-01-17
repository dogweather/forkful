---
title:                "提取子串"
html_title:           "C#: 提取子串"
simple_title:         "提取子串"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c-sharp/extracting-substrings.md"
---

{{< edit_this_page >}}

# 什么 & 为什么？

提取子字符串是指从一个长字符串中截取出特定部分的过程。在编程中，提取子字符串是为了更有效地处理大量的文本数据。

# 如何：

📝 提示：要在C＃中提取子字符串，您可以使用内置的Substring（）方法。以下是一个示例：

```C#
string str = "Hello World";
string sub = str.Substring(6, 5);
Console.WriteLine(sub);
```

👉 输出：World

您可以使用Substring（）方法来提取任意长度的子字符串，其中第一个参数是开始位置，第二个参数是子字符串的长度。

# 深入探索：

📜 历史背景：提取子字符串的概念始于20世纪早期，当时计算机科学家们意识到处理大量文本数据的必要性。

🔀 替代方案：除了使用Substring（）方法外，还有其他几种方便的方式来提取子字符串。例如，在正则表达式中，您可以使用“\b”来匹配单词的边界。

💻 实现细节：在C＃中，Substring（）方法是通过索引和字符数组来实现的。它们利用了字符串的最小单元“char”，并使用指针来迅速定位子字符串。

# 参考资源：

🔗 链接：https://docs.microsoft.com/zh-cn/dotnet/api/system.string.substring

🔗 链接：https://www.tutorialspoint.com/csharp/csharp_string_substring.htm

🔗 链接：https://www.w3schools.com/cs/cs_strings_substring.asp