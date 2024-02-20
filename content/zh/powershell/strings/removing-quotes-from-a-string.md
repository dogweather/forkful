---
date: 2024-01-26 03:41:43.067500-07:00
description: "\u5728PowerShell\u4E2D\u4ECE\u5B57\u7B26\u4E32\u4E2D\u79FB\u9664\u5F15\
  \u53F7\u662F\u6307\u53BB\u9664\u5305\u56F4\u5728\u6587\u672C\u5468\u56F4\u7684\u5355\
  \u5F15\u53F7\uFF08`'`\uFF09\u6216\u53CC\u5F15\u53F7\uFF08`\"`\uFF09\u3002\u7A0B\u5E8F\
  \u5458\u5728\u5904\u7406\u3001\u6BD4\u8F83\u6216\u8F93\u51FA\u5B57\u7B26\u4E32\u65F6\
  \uFF0C\u7279\u522B\u662F\u5728\u5904\u7406\u7528\u6237\u8F93\u5165\u6216\u89E3\u6790\
  \u6587\u4EF6\u65F6\uFF0C\u7ECF\u5E38\u9700\u8981\u6E05\u7406\u5B57\u7B26\u4E32\u3002"
lastmod: 2024-02-19 22:05:07.048051
model: gpt-4-0125-preview
summary: "\u5728PowerShell\u4E2D\u4ECE\u5B57\u7B26\u4E32\u4E2D\u79FB\u9664\u5F15\u53F7\
  \u662F\u6307\u53BB\u9664\u5305\u56F4\u5728\u6587\u672C\u5468\u56F4\u7684\u5355\u5F15\
  \u53F7\uFF08`'`\uFF09\u6216\u53CC\u5F15\u53F7\uFF08`\"`\uFF09\u3002\u7A0B\u5E8F\u5458\
  \u5728\u5904\u7406\u3001\u6BD4\u8F83\u6216\u8F93\u51FA\u5B57\u7B26\u4E32\u65F6\uFF0C\
  \u7279\u522B\u662F\u5728\u5904\u7406\u7528\u6237\u8F93\u5165\u6216\u89E3\u6790\u6587\
  \u4EF6\u65F6\uFF0C\u7ECF\u5E38\u9700\u8981\u6E05\u7406\u5B57\u7B26\u4E32\u3002"
title: "\u4ECE\u5B57\u7B26\u4E32\u4E2D\u79FB\u9664\u5F15\u53F7"
---

{{< edit_this_page >}}

## 什么 & 为什么？
在PowerShell中从字符串中移除引号是指去除包围在文本周围的单引号（`'`）或双引号（`"`）。程序员在处理、比较或输出字符串时，特别是在处理用户输入或解析文件时，经常需要清理字符串。

## 如何操作：
您可以使用`-replace`运算符来去除字符串中的引号。以下是操作方式：

```PowerShell
# 替换单引号
$stringWithSingleQuotes = "'你好，世界！'"
$cleanString = $stringWithSingleQuotes -replace "'", ""
Write-Output $cleanString  # 输出：你好，世界！

# 替换双引号
$stringWithDoubleQuotes = '"你好，世界！"'
$cleanString = $stringWithDoubleQuotes -replace '"', ""
Write-Output $cleanString  # 输出：你好，世界！
```

对于两种引号：

```PowerShell
$stringWithQuotes = '"嗨，那边，" 她说。'
$cleanString = $stringWithQuotes -replace "[\"']", ""  # 注意使用正则表达式字符类
Write-Output $cleanString  # 输出：嗨，那边，她说。
```

控制台中的样本输出将类似于此：

```
你好，世界！
你好，世界！
嗨，那边，她说。
```

## 深入了解
在PowerShell成为微软构思的闪光之前，Windows中的文本处理常常是批处理脚本的领域，这些脚本的能力有限。PowerShell的引入带来了强大的字符串操作特性，使脚本编写变得更加健壮。

除了`-replace`，还有其他方法存在，比如使用`.Trim()`方法仅移除字符串开头和结尾的引号，但它们没有提供相同的控制能力或正则表达式支持。

```PowerShell
# 使用.Trim()删除开头和结尾的引号
$stringWithQuotes = '"你好，世界！"'
$cleanString = $stringWithQuotes.Trim('"')
Write-Output $cleanString  # 输出：你好，世界！
```

请注意，`-replace`在幕后使用正则表达式，因此在使用它时，请记住如果你的目标是特殊字符，需要对其进行转义。如果您需要对引号移除有更细致的控制，深入研究正则表达式与`-replace`是不错的选择，它提供了巨大的灵活性。

## 另请参阅
- 有关PowerShell中正则表达式的更多信息，请查看官方文档：[about_Regular_Expressions](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_regular_expressions?view=powershell-7.1)
- 探索其他字符串方法：[Trim(), TrimStart(), TrimEnd()](https://docs.microsoft.com/en-us/dotnet/api/system.string.trim?view=net-6.0)
