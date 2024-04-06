---
date: 2024-01-20 17:40:58.483339-07:00
description: "\u5982\u4F55\uFF1A \u5982\u679C\u6B63\u786E\u65E0\u8BEF\uFF0C\u4F60\u4F1A\
  \u770B\u5230\uFF1A 1. \u521B\u5EFA\u7684\u4E34\u65F6\u6587\u4EF6\u8DEF\u5F84 2.\
  \ \"Hello, temporary world!\"\u6587\u672C 3. \u7136\u540E\u6587\u4EF6\u6D88\u5931\
  \u4E0D\u89C1\u4E86."
isCJKLanguage: true
lastmod: '2024-04-05T22:38:47.192670-06:00'
model: gpt-4-1106-preview
summary: "\u5982\u4F55\uFF1A \u5982\u679C\u6B63\u786E\u65E0\u8BEF\uFF0C\u4F60\u4F1A\
  \u770B\u5230\uFF1A 1. \u521B\u5EFA\u7684\u4E34\u65F6\u6587\u4EF6\u8DEF\u5F84 2.\
  \ \"Hello, temporary world!\"\u6587\u672C 3. \u7136\u540E\u6587\u4EF6\u6D88\u5931\
  \u4E0D\u89C1\u4E86."
title: "\u521B\u5EFA\u4E34\u65F6\u6587\u4EF6"
weight: 21
---

## 如何：
```PowerShell
# 创建一个临时文件
$tempFile = [System.IO.Path]::GetTempFileName()
# 查看临时文件名字
$tempFile

# 写点东西到临时文件
"Hello, temporary world!" | Out-File -FilePath $tempFile

# 读取临时文件内容
Get-Content -Path $tempFile

# 删掉临时文件
Remove-Item -Path $tempFile
```

如果正确无误，你会看到：
1. 创建的临时文件路径
2. "Hello, temporary world!"文本
3. 然后文件消失不见了

## 深入探讨
早年，程序员处理数据经常需要创建临时存储位置。在PowerShell里，利用.NET库可以很容易地创建临时文件，这是现在的常用方法了。另外选项，比如`New-TemporaryFile` cmdlet，也可以但功能较少。再实际操作中，创建临时文件的方式要考虑到安全性、容错性还有是否能顺畅清除。

## 参看
- [System.IO.Path .NET API 文档](https://docs.microsoft.com/zh-cn/dotnet/api/system.io.path?view=net-6.0)
- [Out-File cmdlet 使用指南](https://docs.microsoft.com/zh-cn/powershell/module/microsoft.powershell.utility/out-file?view=powershell-7)
