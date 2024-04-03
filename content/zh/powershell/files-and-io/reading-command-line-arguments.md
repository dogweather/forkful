---
date: 2024-01-20 17:56:29.725252-07:00
description: "How to: (\u5982\u4F55\u64CD\u4F5C\uFF1A) \u4E0B\u9762\u7684\u793A\u4F8B\
  \u663E\u793A\u4E86\u5982\u4F55\u5728 PowerShell \u4E2D\u8BBF\u95EE\u547D\u4EE4\u884C\
  \u53C2\u6570\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:48.031245-06:00'
model: gpt-4-1106-preview
summary: "\u4E0B\u9762\u7684\u793A\u4F8B\u663E\u793A\u4E86\u5982\u4F55\u5728 PowerShell\
  \ \u4E2D\u8BBF\u95EE\u547D\u4EE4\u884C\u53C2\u6570."
title: "\u8BFB\u53D6\u547D\u4EE4\u884C\u53C2\u6570"
weight: 23
---

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
