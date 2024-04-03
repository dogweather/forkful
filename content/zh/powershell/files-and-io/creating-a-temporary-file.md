---
date: 2024-01-20 17:40:58.483339-07:00
description: "\u521B\u5EFA\u4E34\u65F6\u6587\u4EF6\u5728\u7F16\u7A0B\u4E2D\u5F88\u5E38\
  \u89C1\uFF0C\u5B83\u5C31\u662F\u5236\u4F5C\u4E00\u4E2A\u77ED\u6682\u5B58\u5728\u7684\
  \u6587\u4EF6\uFF0C\u7528\u6765\u5904\u7406\u4E34\u65F6\u6570\u636E\u6216\u8005\u5F53\
  \u4F5C\u4E00\u4E2A\u64CD\u4F5C\u7684\u5907\u4EFD\u3002\u7A0B\u5E8F\u5458\u8FD9\u4E48\
  \u505A\u662F\u4E3A\u4E86\u4E0D\u5E72\u6270\u6B63\u5F0F\u6587\u4EF6\uFF0C\u5904\u7406\
  \u5B8C\u5C31\u4E22\u6389\uFF0C\u4FDD\u6301\u73AF\u5883\u6E05\u6D01\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:48.036228-06:00'
model: gpt-4-1106-preview
summary: "\u521B\u5EFA\u4E34\u65F6\u6587\u4EF6\u5728\u7F16\u7A0B\u4E2D\u5F88\u5E38\
  \u89C1\uFF0C\u5B83\u5C31\u662F\u5236\u4F5C\u4E00\u4E2A\u77ED\u6682\u5B58\u5728\u7684\
  \u6587\u4EF6\uFF0C\u7528\u6765\u5904\u7406\u4E34\u65F6\u6570\u636E\u6216\u8005\u5F53\
  \u4F5C\u4E00\u4E2A\u64CD\u4F5C\u7684\u5907\u4EFD\u3002\u7A0B\u5E8F\u5458\u8FD9\u4E48\
  \u505A\u662F\u4E3A\u4E86\u4E0D\u5E72\u6270\u6B63\u5F0F\u6587\u4EF6\uFF0C\u5904\u7406\
  \u5B8C\u5C31\u4E22\u6389\uFF0C\u4FDD\u6301\u73AF\u5883\u6E05\u6D01\u3002."
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
