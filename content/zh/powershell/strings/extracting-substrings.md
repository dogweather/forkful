---
date: 2024-01-20 17:46:37.892660-07:00
description: "\u63D0\u53D6\u5B50\u5B57\u7B26\u4E32\u5C31\u662F\u4ECE\u4E00\u4E2A\u957F\
  \u5B57\u7B26\u4E32\u4E2D\u622A\u53D6\u51FA\u4E00\u90E8\u5206\u5185\u5BB9\u3002\u7A0B\
  \u5E8F\u5458\u8FD9\u4E48\u505A\u662F\u4E3A\u4E86\u83B7\u53D6\u91CD\u8981\u7684\u6570\
  \u636E\uFF0C\u6E05\u6D17\u6587\u672C\u6216\u8005\u7B80\u5316\u4FE1\u606F\u5904\u7406\
  \u3002"
isCJKLanguage: true
lastmod: 2024-02-19 22:05:07.049134
model: gpt-4-1106-preview
summary: "\u63D0\u53D6\u5B50\u5B57\u7B26\u4E32\u5C31\u662F\u4ECE\u4E00\u4E2A\u957F\
  \u5B57\u7B26\u4E32\u4E2D\u622A\u53D6\u51FA\u4E00\u90E8\u5206\u5185\u5BB9\u3002\u7A0B\
  \u5E8F\u5458\u8FD9\u4E48\u505A\u662F\u4E3A\u4E86\u83B7\u53D6\u91CD\u8981\u7684\u6570\
  \u636E\uFF0C\u6E05\u6D17\u6587\u672C\u6216\u8005\u7B80\u5316\u4FE1\u606F\u5904\u7406\
  \u3002"
title: "\u63D0\u53D6\u5B50\u5B57\u7B26\u4E32"
---

{{< edit_this_page >}}

## What & Why? (是什么？为什么？)
提取子字符串就是从一个长字符串中截取出一部分内容。程序员这么做是为了获取重要的数据，清洗文本或者简化信息处理。

## How to: (如何操作)
```PowerShell
# 简单示例
$text = "Hello, PowerShell!"
# 从第7个字符开始提取5个字符
$substring = $text.Substring(7, 5)
Write-Output $substring # 输出：Power

# 使用范围运算符
$rangeSubstring = $text[7..11] -join ''
Write-Output $rangeSubstring # 输出：Power

# 用正则表达式匹配
$regexSubstring = [regex]::Match($text, 'Power').Value
Write-Output $regexSubstring # 输出：Power
```

## Deep Dive (深入探究)
提取子字符串的功能在多数编程语言都很常见，PowerShell也不例外。过去，我们依靠.NET框架的`Substring`方法来实现，现在有了PowerShell的范围运算符，操作更直观。正则表达式则提供了一种强大的模式匹配能力，对于复杂字符串处理尤其有用。在实际工作中，选择哪种方法取决于具体需求：`Substring`适合简单精确的裁剪，范围运算符则在处理字符数组时表现更佳，而正则表达式的优势在于处理复杂的文本模式。

## See Also (另请参阅)
- [Microsoft's official documentation on String.Substring Method](https://docs.microsoft.com/en-us/dotnet/api/system.string.substring?view=net-6.0)
- [About Array slice ranges in PowerShell](https://docs.microsoft.com/en-us/powershell/scripting/learn/deep-dives/everything-about-arrays?view=powershell-7.1#slicing-arrays)
- [Regular Expressions in .NET](https://docs.microsoft.com/en-us/dotnet/standard/base-types/regular-expressions)
- [PowerShell Gallery Scripts for String Manipulation](https://www.powershellgallery.com/)
