---
title:                "创建临时文件"
date:                  2024-01-20T17:40:58.483339-07:00
model:                 gpt-4-1106-preview
simple_title:         "创建临时文件"

tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/powershell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？
创建临时文件在编程中很常见，它就是制作一个短暂存在的文件，用来处理临时数据或者当作一个操作的备份。程序员这么做是为了不干扰正式文件，处理完就丢掉，保持环境清洁。

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
