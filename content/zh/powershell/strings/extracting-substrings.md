---
date: 2024-01-20 17:46:37.892660-07:00
description: null
isCJKLanguage: true
lastmod: '2024-04-05T21:53:48.296880-06:00'
model: gpt-4-1106-preview
summary: "(\u5982\u4F55\u64CD\u4F5C) \u63D0\u53D6\u5B50\u5B57\u7B26\u4E32\u7684\u529F\
  \u80FD\u5728\u591A\u6570\u7F16\u7A0B\u8BED\u8A00\u90FD\u5F88\u5E38\u89C1\uFF0CPowerShell\u4E5F\
  \u4E0D\u4F8B\u5916\u3002\u8FC7\u53BB\uFF0C\u6211\u4EEC\u4F9D\u9760.NET\u6846\u67B6\
  \u7684`Substring`\u65B9\u6CD5\u6765\u5B9E\u73B0\uFF0C\u73B0\u5728\u6709\u4E86PowerShell\u7684\
  \u8303\u56F4\u8FD0\u7B97\u7B26\uFF0C\u64CD\u4F5C\u66F4\u76F4\u89C2\u3002\u6B63\u5219\
  \u8868\u8FBE\u5F0F\u5219\u63D0\u4F9B\u4E86\u4E00\u79CD\u5F3A\u5927\u7684\u6A21\u5F0F\
  \u5339\u914D\u80FD\u529B\uFF0C\u5BF9\u4E8E\u590D\u6742\u5B57\u7B26\u4E32\u5904\u7406\
  \u5C24\u5176\u6709\u7528\u3002\u5728\u5B9E\u9645\u5DE5\u4F5C\u4E2D\uFF0C\u9009\u62E9\
  \u54EA\u79CD\u65B9\u6CD5\u53D6\u51B3\u4E8E\u5177\u4F53\u9700\u6C42\uFF1A`Substring`\u9002\
  \u5408\u7B80\u5355\u7CBE\u786E\u7684\u88C1\u526A\uFF0C\u8303\u56F4\u8FD0\u7B97\u7B26\
  \u5219\u5728\u5904\u7406\u5B57\u7B26\u6570\u7EC4\u65F6\u8868\u73B0\u66F4\u4F73\uFF0C\
  \u800C\u6B63\u5219\u8868\u8FBE\u5F0F\u7684\u4F18\u52BF\u5728\u4E8E\u5904\u7406\u590D\
  \u6742\u7684\u6587\u672C\u6A21\u5F0F\u3002"
title: "\u63D0\u53D6\u5B50\u5B57\u7B26\u4E32"
weight: 6
---

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
