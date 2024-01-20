---
title:                "使用正则表达式"
html_title:           "C: 使用正则表达式"
simple_title:         "使用正则表达式"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/powershell/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why? 
什么 & 为什么？

正则表达式是一种强大的文本模式匹配和查找工具。程序员用它来搜索、编辑或处理复杂文本和数据结构，以节省时间和增加代码的灵活性。

## How to:
怎么做：

```PowerShell
# 查找数字
$text = "订单号12345和54321随时准备发货。"
$matches = [regex]::Matches($text, '\d+')
$matches.Value

# 输出
12345
54321

# 替换字符串
$text = "这是一个old item。"
$newText = $text -replace 'old', 'new'
$newText

# 输出
这是一个new item。
```

## Deep Dive
深入探究：

正则表达式起源于20世纪50年代的自动理论和形式语言。针对不同编程问题，你可以选择字符串函数或解析器作为替代方案。在PowerShell中，regexp是.NET的System.Text.RegularExpressions命名空间底下实现的，这意味着它们非常强大且高效。

## See Also
另请参阅：

- [Microsoft Docs: about_Regular_Expressions](https://docs.microsoft.com/powershell/module/microsoft.powershell.core/about/about_regular_expressions)
- [Regular-Expressions.info](https://www.regular-expressions.info/powershell.html)
- [.NET System.Text.RegularExpressions Namespace](https://docs.microsoft.com/dotnet/api/system.text.regularexpressions?view=netframework-4.8)