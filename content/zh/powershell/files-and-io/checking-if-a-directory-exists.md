---
title:                "检查目录是否存在"
date:                  2024-02-03T19:08:11.483516-07:00
model:                 gpt-4-0125-preview
simple_title:         "检查目录是否存在"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/powershell/checking-if-a-directory-exists.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 什么与为什么？
在 PowerShell 中，检查目录是否存在是一个常见的任务，它帮助脚本根据文件系统结构做出决策——例如，在尝试读取或写入之前确认目标目录是否就绪以避免错误。这对于确保您的脚本在不同环境中可靠运行至关重要。

## 如何进行：
PowerShell 提供了一个直接的方法来检查目录是否存在，使用的是 `Test-Path` cmdlet。此 cmdlet 返回一个布尔值，指示指定的路径是否存在。以下是如何使用它的方法：

```powershell
# 检查目录是否存在
$directoryPath = "C:\ExamplePath"
$directoryExists = Test-Path -Path $directoryPath
Write-Output "目录是否存在？$directoryExists"
```

对于存在的目录，示例输出为：

```
目录是否存在？True
```

而对于不存在的目录：

```
目录是否存在？False
```

对于更复杂的脚本，特别是那些与网络共享或云存储交互的脚本，您可能需要额外的检查或功能，这些功能通过 `Test-Path` 直接提供是不够的。在这种情况下，使用第三方 PowerShell 模块或库可能会有所帮助，尽管大多数常规任务可以通过 PowerShell 的内置 cmdlet 完成。截至我的最后知识更新，尚未有一个广泛采用的第三方库专门用于检查目录存在性，超出 `Test-Path` 提供的范围，主要是因为 `Test-Path` 本身就是既健壮又高效的。
