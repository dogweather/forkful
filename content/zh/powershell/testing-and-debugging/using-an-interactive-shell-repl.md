---
date: 2024-01-26 04:16:50.929403-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u542F\u52A8PowerShell\uFF0C\u4F60\u5C31\
  \u8FDB\u5165\u4E86REPL\u3002\u5C1D\u8BD5\u4F7F\u7528`Get-Date` Cmdlet\uFF1A."
lastmod: '2024-04-05T21:53:48.312581-06:00'
model: gpt-4-0125-preview
summary: ''
title: "\u5728\u7F16\u7A0B\u4E2D\u4F7F\u7528\u4EA4\u4E92\u5F0FShell\uFF08REPL\uFF09"
weight: 34
---

## 如何操作：
启动PowerShell，你就进入了REPL。尝试使用`Get-Date` Cmdlet：

```PowerShell
PS > Get-Date
```

你应该会看到当前日期和时间的输出：

```PowerShell
2023年3月31日 星期三 12:34:56 PM
```

现在，串联命令。让我们按内存使用量对进程进行排序：

```PowerShell
PS > Get-Process | Sort-Object WS -Descending | Select-Object -First 5
```

这将输出按工作集大小（内存使用量）排名前5的进程。

## 深入探讨
PowerShell的REPL根源于Unix shell和其他动态语言shell，如Python的。它是一个单用户的交互式命令执行环境。与你编写整个应用程序然后编译的编译型语言不同，REPL环境让你可以一次写一行代码进行运行。对于更大的任务，PowerShell也支持脚本执行。

Windows的替代选项包括命令提示符或其他特定语言的REPL，如IPython。在Unix/Linux世界中，像bash或zsh这样的shell执行类似的功能。

PowerShell的实现使用宿主应用程序来运行shell。虽然Windows中的PowerShell.exe是最常见的，但其他诸如集成脚本环境（ISE）或Visual Studio Code的集成终端也可以作为宿主。

## 另请参阅
- [关于PowerShell](https://docs.microsoft.com/en-us/powershell/scripting/overview)
- [StackOverflow：PowerShell](https://stackoverflow.com/questions/tagged/powershell)
