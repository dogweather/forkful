---
title:                "C#: 使用正则表达式"
simple_title:         "使用正则表达式"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c-sharp/using-regular-expressions.md"
---

{{< edit_this_page >}}

为什么：正则表达式是一种强大的工具，可以用来处理和匹配字符串模式。它可以帮助开发人员在处理文本时更有效率，减少大量的手动工作。 

如何使用： 

```C# 
// 导入System.Text.RegularExpressions命名空间 
using System.Text.RegularExpressions; 

// 定义一个包含混合大小写和数字的字符串 
string myString = "Th1sIsAMiXeDStr1ng"; 

// 创建一个正则表达式匹配模式，匹配字符串中的数字 
Regex pattern = new Regex(@"\d"); 

// 使用Matches函数返回匹配结果集合 
MatchCollection matches = pattern.Matches(myString); 

// 循环遍历结果集合并输出每个匹配项 
foreach (Match match in matches) 
{ 
    Console.WriteLine(match.Value); 
} 
``` 

上面的代码将输出： 1 1。 

深入了解：正则表达式可以使用一些特殊的语法来匹配不同的模式，例如在上面示例中使用的“\d”来匹配数字。还可以使用量词来匹配特定数量的字符，或使用字符类来匹配特定类型的字符。此外，使用特殊的符号和命令可以实现更高级的模式匹配功能。要学习更多关于正则表达式的知识，请查阅下面的参考链接。 

参考链接： 

[正则表达式入门教程] (https://www.runoob.com/regexp/regexp-tutorial.html) 
[.NET正则表达式指南] (https://docs.microsoft.com/en-us/dotnet/standard/base-types/regular-expression-language-quick-reference) 
[Regexr在线工具] (https://regexr.com/) 

另请参阅：