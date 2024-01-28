---
title:                "从字符串中移除引号"
date:                  2024-01-26T03:41:43.067500-07:00
model:                 gpt-4-0125-preview
simple_title:         "从字符串中移除引号"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/powershell/removing-quotes-from-a-string.md"
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
