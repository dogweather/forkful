---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:08:11.483516-07:00
description: "\u5728 PowerShell \u4E2D\uFF0C\u68C0\u67E5\u76EE\u5F55\u662F\u5426\u5B58\
  \u5728\u662F\u4E00\u4E2A\u5E38\u89C1\u7684\u4EFB\u52A1\uFF0C\u5B83\u5E2E\u52A9\u811A\
  \u672C\u6839\u636E\u6587\u4EF6\u7CFB\u7EDF\u7ED3\u6784\u505A\u51FA\u51B3\u7B56\u2014\
  \u2014\u4F8B\u5982\uFF0C\u5728\u5C1D\u8BD5\u8BFB\u53D6\u6216\u5199\u5165\u4E4B\u524D\
  \u786E\u8BA4\u76EE\u6807\u76EE\u5F55\u662F\u5426\u5C31\u7EEA\u4EE5\u907F\u514D\u9519\
  \u8BEF\u3002\u8FD9\u5BF9\u4E8E\u786E\u4FDD\u60A8\u7684\u811A\u672C\u5728\u4E0D\u540C\
  \u73AF\u5883\u4E2D\u53EF\u9760\u8FD0\u884C\u81F3\u5173\u91CD\u8981\u3002"
lastmod: '2024-03-13T22:44:48.030184-06:00'
model: gpt-4-0125-preview
summary: "\u5728 PowerShell \u4E2D\uFF0C\u68C0\u67E5\u76EE\u5F55\u662F\u5426\u5B58\
  \u5728\u662F\u4E00\u4E2A\u5E38\u89C1\u7684\u4EFB\u52A1\uFF0C\u5B83\u5E2E\u52A9\u811A\
  \u672C\u6839\u636E\u6587\u4EF6\u7CFB\u7EDF\u7ED3\u6784\u505A\u51FA\u51B3\u7B56\u2014\
  \u2014\u4F8B\u5982\uFF0C\u5728\u5C1D\u8BD5\u8BFB\u53D6\u6216\u5199\u5165\u4E4B\u524D\
  \u786E\u8BA4\u76EE\u6807\u76EE\u5F55\u662F\u5426\u5C31\u7EEA\u4EE5\u907F\u514D\u9519\
  \u8BEF\u3002\u8FD9\u5BF9\u4E8E\u786E\u4FDD\u60A8\u7684\u811A\u672C\u5728\u4E0D\u540C\
  \u73AF\u5883\u4E2D\u53EF\u9760\u8FD0\u884C\u81F3\u5173\u91CD\u8981\u3002"
title: "\u68C0\u67E5\u76EE\u5F55\u662F\u5426\u5B58\u5728"
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
