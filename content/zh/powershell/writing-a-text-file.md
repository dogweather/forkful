---
title:                "编写文本文件"
date:                  2024-01-19
html_title:           "Arduino: 编写文本文件"
simple_title:         "编写文本文件"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/powershell/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?
写文本文件是把字符串存到文件里。程序员这么做是因为需要保存设置、数据或者日志。

## How to:
创建和写入文本文件，用`Out-File`和`Set-Content`命令。
```PowerShell
# 使用Out-File
"欢迎来到PowerShell世界！" | Out-File -FilePath .\example.txt

# 查看文件内容
Get-Content .\example.txt

# 使用Set-Content
"开始PowerShell之旅。" | Set-Content -Path .\example.txt

# 再次查看文件内容
Get-Content .\example.txt
```

## Deep Dive
以前，PowerShell用`>`和`>>`操作符来写文件。`Out-File`和`Set-Content`是更现代的选项。`Out-File`更适合输出命令结果，`Set-Content`适合写简简单单的文字。

## See Also
- [Out-File command help](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/out-file?view=powershell-7.1)
- [Set-Content command help](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.management/set-content?view=powershell-7.1)
