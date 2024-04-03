---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:06:02.241237-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A PowerShell \u4F5C\u4E3A\u4E00\u4E2A\u591A\
  \u529F\u80FD\u5DE5\u5177\uFF0C\u5141\u8BB8\u60A8\u4F7F\u7528\u7B80\u5355\u7684\u65B9\
  \u6CD5\u5BF9\u5B57\u7B26\u4E32\u8FDB\u884C\u9996\u5B57\u6BCD\u5927\u5199\uFF0C\u65E0\
  \u9700\u7B2C\u4E09\u65B9\u5E93\u3002\u4EE5\u4E0B\u662F\u5982\u4F55\u505A\u5230\u8FD9\
  \u4E00\u70B9\uFF1A."
lastmod: '2024-03-13T22:44:47.992553-06:00'
model: gpt-4-0125-preview
summary: "PowerShell \u4F5C\u4E3A\u4E00\u4E2A\u591A\u529F\u80FD\u5DE5\u5177\uFF0C\u5141\
  \u8BB8\u60A8\u4F7F\u7528\u7B80\u5355\u7684\u65B9\u6CD5\u5BF9\u5B57\u7B26\u4E32\u8FDB\
  \u884C\u9996\u5B57\u6BCD\u5927\u5199\uFF0C\u65E0\u9700\u7B2C\u4E09\u65B9\u5E93\u3002\
  \u4EE5\u4E0B\u662F\u5982\u4F55\u505A\u5230\u8FD9\u4E00\u70B9\uFF1A."
title: "\u5B57\u7B26\u4E32\u5927\u5199\u5316"
weight: 2
---

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
