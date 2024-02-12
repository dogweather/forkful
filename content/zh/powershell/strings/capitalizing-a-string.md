---
title:                "字符串大写化"
aliases:
- /zh/powershell/capitalizing-a-string/
date:                  2024-02-03T19:06:02.241237-07:00
model:                 gpt-4-0125-preview
simple_title:         "字符串大写化"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/powershell/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 什么 & 为什么？
在 PowerShell 中，将字符串首字母大写涉及到将给定字符串的第一个字符转换为大写，同时保留字符串的其余部分不变。程序员经常执行此任务以达到格式化目的，例如为用户界面显示准备文本或在生成的文档中遵循语法规则。

## 如何操作：
PowerShell 作为一个多功能工具，允许您使用简单的方法对字符串进行首字母大写，无需第三方库。以下是如何做到这一点：

```powershell
# 使用内置的 .Net 方法 'ToTitleCase' 从 CultureInfo
$text = "hello world"
$culture = [System.Globalization.CultureInfo]::InvariantCulture
$capitalizedText = $culture.TextInfo.ToTitleCase($text.ToLower())
Write-Output $capitalizedText
```
输出：
```
Hello world
```

注意：此方法将每个单词的第一个字母大写。如果您严格地只想将字符串的第一个字母大写，并保留其余部分不变，您可以这样做：

```powershell
# 仅将字符串的第一个字符大写
$text = "hello world"
$capitalizedText = $text.Substring(0,1).ToUpper() + $text.Substring(1)
Write-Output $capitalizedText
```
输出：
```
Hello world
```

PowerShell 直接不包括一个简单的函数来仅将字符串的第一个字母大写，但通过组合基本的字符串操作方法，如 `Substring(0,1).ToUpper()` 和连接，我们可以轻松实现预期结果。
