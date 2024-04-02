---
date: 2024-01-20 17:56:29.725252-07:00
description: "\u5728 PowerShell \u811A\u672C\u4E2D\u8BFB\u53D6\u547D\u4EE4\u884C\u53C2\
  \u6570\u53EF\u4EE5\u8BA9\u4F60\u7684\u811A\u672C\u6839\u636E\u7528\u6237\u8F93\u5165\
  \u5B9A\u5236\u64CD\u4F5C\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\u4E3A\u4E86\u589E\
  \u52A0\u811A\u672C\u7684\u7075\u6D3B\u6027\u548C\u5B9E\u7528\u6027\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:48.031245-06:00'
model: gpt-4-1106-preview
summary: "\u5728 PowerShell \u811A\u672C\u4E2D\u8BFB\u53D6\u547D\u4EE4\u884C\u53C2\
  \u6570\u53EF\u4EE5\u8BA9\u4F60\u7684\u811A\u672C\u6839\u636E\u7528\u6237\u8F93\u5165\
  \u5B9A\u5236\u64CD\u4F5C\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\u4E3A\u4E86\u589E\
  \u52A0\u811A\u672C\u7684\u7075\u6D3B\u6027\u548C\u5B9E\u7528\u6027\u3002"
title: "\u8BFB\u53D6\u547D\u4EE4\u884C\u53C2\u6570"
weight: 23
---

## What & Why? (是什么？为什么？)
在 PowerShell 脚本中读取命令行参数可以让你的脚本根据用户输入定制操作。程序员这样做为了增加脚本的灵活性和实用性。

## How to: (如何操作：)
下面的示例显示了如何在 PowerShell 中访问命令行参数。

```PowerShell
# myscript.ps1

# 打印所有命令行参数
Write-Host "所有参数：$args"

# 打印第一个参数
Write-Host "第一个参数：$args[0]"
```

运行 `.\myscript.ps1 arg1 arg2 arg3` 后的输出：

```PowerShell
所有参数：arg1 arg2 arg3
第一个参数：arg1
```

## Deep Dive (深入了解)
在早期的 shell 编程中，命令行参数就被用来影响脚本的行为。PowerShell 在这方面继承了许多传统的 Shell 特性。除了使用 `$args`，你还可以利用 `param` 声明来定义形式参数，这可以提供更明确的参数处理方式，例如：

```PowerShell
# myscript.ps1

param (
  [String]$name,
  [Int]$age
)

Write-Host "姓名：$name"
Write-Host "年龄：$age"
```

从 PowerShell 2.0 开始，还可以使用高级功能，比如 `[CmdletBinding()]` 和 `param` 块来创建更复杂的脚本参数声明。

## See Also (另请参阅)
- [about_Functions_Advanced_Parameters (高级参数)](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_Functions_Advanced_Parameters)
- [about_Parameters (参数)](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_Parameters)
