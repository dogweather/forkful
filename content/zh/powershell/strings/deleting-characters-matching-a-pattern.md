---
date: 2024-01-20 17:42:53.386096-07:00
description: "How to: \u600E\u4E48\u505A\uFF1F \u5728 PowerShell, `-replace` \u64CD\
  \u4F5C\u7B26\u4F7F\u7528\u6B63\u5219\u8868\u8FBE\u5F0F\u6765\u5339\u914D\u548C\u66FF\
  \u6362\u6587\u672C\u3002\u5B83\u662F\u57FA\u4E8E .NET Framework \u7684\uFF0C\u6240\
  \u4EE5\u6027\u80FD\u53EF\u9760\u3002\u4F20\u7EDF\u4E0A\uFF0C\u5B57\u7B26\u5904\u7406\
  \u91CC\u6709\u5176\u4ED6\u65B9\u5F0F\u5982 `String.Replace()` \u65B9\u6CD5\uFF0C\
  \u4F46 `-replace`\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:48.291859-06:00'
model: gpt-4-1106-preview
summary: "\u600E\u4E48\u505A\uFF1F \u5728 PowerShell, `-replace` \u64CD\u4F5C\u7B26\
  \u4F7F\u7528\u6B63\u5219\u8868\u8FBE\u5F0F\u6765\u5339\u914D\u548C\u66FF\u6362\u6587\
  \u672C\u3002\u5B83\u662F\u57FA\u4E8E .NET Framework \u7684\uFF0C\u6240\u4EE5\u6027\
  \u80FD\u53EF\u9760\u3002\u4F20\u7EDF\u4E0A\uFF0C\u5B57\u7B26\u5904\u7406\u91CC\u6709\
  \u5176\u4ED6\u65B9\u5F0F\u5982 `String.Replace()` \u65B9\u6CD5\uFF0C\u4F46 `-replace`\
  \ \u66F4\u7075\u6D3B\u3002\u5B9E\u73B0\u7684\u65F6\u5019\uFF0C\u5B83\u9996\u5148\
  \u5206\u6790\u6A21\u5F0F\uFF0C\u7136\u540E\u641C\u7D22\u5339\u914D\u4E32\uFF0C\u63A5\
  \u7740\u8FDB\u884C\u6587\u672C\u66FF\u6362\u3002"
title: "\u5339\u914D\u6A21\u5F0F\u5220\u9664\u5B57\u7B26"
weight: 5
---

## How to: 怎么做？
```PowerShell
# 示例：用正则表达式移除数字
$text = "PowerShell版本7.2.0"
$pattern = '[0-9]+'
$text -replace $pattern, ''
```
输出：
```
PowerShell版本..
```

```PowerShell
# 示例：删除特定单词
$phrase = "删除这个词 - 删除"
$word = "删除"
$phrase -replace $word, ''
```
输出：
```
这个词 - 
```

## Deep Dive 深入探索
在 PowerShell, `-replace` 操作符使用正则表达式来匹配和替换文本。它是基于 .NET Framework 的，所以性能可靠。传统上，字符处理里有其他方式如 `String.Replace()` 方法，但 `-replace` 更灵活。实现的时候，它首先分析模式，然后搜索匹配串，接着进行文本替换。

## See Also 另请参阅
- [正则表达式简介](https://docs.microsoft.com/zh-cn/dotnet/standard/base-types/regular-expression-language-quick-reference)
- [PowerShell替换操作符文档](https://docs.microsoft.com/powershell/module/microsoft.powershell.core/about/about_comparison_operators?view=powershell-7.2#replace-operator)
- [.NET 的 `Regex` 类文档](https://docs.microsoft.com/zh-cn/dotnet/api/system.text.regularexpressions.regex?view=net-6.0)
