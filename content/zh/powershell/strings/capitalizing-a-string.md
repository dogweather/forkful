---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:06:02.241237-07:00
description: "\u5728 PowerShell \u4E2D\uFF0C\u5C06\u5B57\u7B26\u4E32\u9996\u5B57\u6BCD\
  \u5927\u5199\u6D89\u53CA\u5230\u5C06\u7ED9\u5B9A\u5B57\u7B26\u4E32\u7684\u7B2C\u4E00\
  \u4E2A\u5B57\u7B26\u8F6C\u6362\u4E3A\u5927\u5199\uFF0C\u540C\u65F6\u4FDD\u7559\u5B57\
  \u7B26\u4E32\u7684\u5176\u4F59\u90E8\u5206\u4E0D\u53D8\u3002\u7A0B\u5E8F\u5458\u7ECF\
  \u5E38\u6267\u884C\u6B64\u4EFB\u52A1\u4EE5\u8FBE\u5230\u683C\u5F0F\u5316\u76EE\u7684\
  \uFF0C\u4F8B\u5982\u4E3A\u7528\u6237\u754C\u9762\u663E\u793A\u51C6\u5907\u6587\u672C\
  \u6216\u5728\u751F\u6210\u7684\u6587\u6863\u4E2D\u9075\u5FAA\u8BED\u6CD5\u89C4\u5219\
  \u3002"
lastmod: '2024-03-13T22:44:47.992553-06:00'
model: gpt-4-0125-preview
summary: "\u5728 PowerShell \u4E2D\uFF0C\u5C06\u5B57\u7B26\u4E32\u9996\u5B57\u6BCD\
  \u5927\u5199\u6D89\u53CA\u5230\u5C06\u7ED9\u5B9A\u5B57\u7B26\u4E32\u7684\u7B2C\u4E00\
  \u4E2A\u5B57\u7B26\u8F6C\u6362\u4E3A\u5927\u5199\uFF0C\u540C\u65F6\u4FDD\u7559\u5B57\
  \u7B26\u4E32\u7684\u5176\u4F59\u90E8\u5206\u4E0D\u53D8\u3002\u7A0B\u5E8F\u5458\u7ECF\
  \u5E38\u6267\u884C\u6B64\u4EFB\u52A1\u4EE5\u8FBE\u5230\u683C\u5F0F\u5316\u76EE\u7684\
  \uFF0C\u4F8B\u5982\u4E3A\u7528\u6237\u754C\u9762\u663E\u793A\u51C6\u5907\u6587\u672C\
  \u6216\u5728\u751F\u6210\u7684\u6587\u6863\u4E2D\u9075\u5FAA\u8BED\u6CD5\u89C4\u5219\
  \u3002."
title: "\u5B57\u7B26\u4E32\u5927\u5199\u5316"
weight: 2
---

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
