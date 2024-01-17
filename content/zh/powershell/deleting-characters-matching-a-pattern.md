---
title:                "删除匹配模式的字符"
html_title:           "PowerShell: 删除匹配模式的字符"
simple_title:         "删除匹配模式的字符"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/powershell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 是什么 & 为什么？
删除匹配模式的字符是指删除字符串中符合特定模式的字符。程序员们经常这样做是为了减少字符串中的无用字符，使数据更干净、更易于处理。

## 如何：
下面是几个在 PowerShell 中删除匹配模式的字符的代码示例。每个代码示例后面都有对用法和预期的输出的解释。

### 删除整个单词
```
$str = "Hello World!"
$str -replace '\b\w+\b'
```
上述代码将删除字符串“Hello World！”中的所有单词，预期输出是一个空字符串。

### 删除所有的数字
```
$str = "123abc456def"
$str -replace '\d+'
```
上述代码将删除字符串“123abc456def”中的所有数字，预期输出是字符串“abcdef”。

### 删除邮箱地址中的用户名
```
$str = "example@mail.com"
$str -replace '.*@'
```
上述代码将删除字符串“example@mail.com”中的用户名“example@”，预期输出是邮箱地址的域名“mail.com”。

## 深入了解：
删除字符的第一个记录可追溯到 Unix 的编辑器“ed”，后来又出现了能够查找和替换字符的 “grep” 和 “sed” 命令。在 PowerShell 中，除了使用 `-replace` 的方法，还可以使用具有相同功能的命令 “Select-String”。

其他替代方法包括使用正则表达式、substring 和 split 函数。另外，可以使用 PowerShell 的.NET Framework 类来操作字符串，例如使用 `System.Text.RegularExpressions` 类中的 `Regex.Replace()` 方法来删除字符。

在实现上，删除字符的模式匹配基于正则表达式引擎，正则表达式使用 ECMAscript 语法。这意味着它与 JavaScript 正则表达式兼容，但不一定与其他编程语言的正则表达式兼容。

## 参考链接：
- PowerShell 的官方文档：https://docs.microsoft.com/zh-cn/powershell/
- 在 PowerShell 中使用正则表达式：https://docs.microsoft.com/zh-cn/powershell/module/microsoft.powershell.core/about/about_regular_expressions?view=powershell-7
- 使用.NET Framework 操作字符串：https://docs.microsoft.com/zh-cn/dotnet/standard/base-types/regular-expression-language-quick-reference