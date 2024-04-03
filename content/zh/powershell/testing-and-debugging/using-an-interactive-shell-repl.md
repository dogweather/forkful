---
date: 2024-01-26 04:16:50.929403-07:00
description: "\u4EA4\u4E92\u5F0Fshell\uFF0C\u6216\u8BFB\u53D6-\u6C42\u503C-\u6253\u5370\
  \u5FAA\u73AF\uFF08REPL\uFF09\uFF0C\u8BA9\u4F60\u80FD\u591F\u8F93\u5165PowerShell\u547D\
  \u4EE4\u5E76\u7ACB\u5373\u83B7\u53D6\u53CD\u9988\u3002\u7A0B\u5E8F\u5458\u4F7F\u7528\
  \u5B83\u6765\u5FEB\u901F\u6D4B\u8BD5\u4EE3\u7801\u7247\u6BB5\u3001\u8C03\u8BD5\u6216\
  \u5728\u4E0D\u7F16\u5199\u5B8C\u6574\u811A\u672C\u7684\u60C5\u51B5\u4E0B\u5B66\u4E60\
  \u65B0\u547D\u4EE4\u3002"
lastmod: '2024-03-13T22:44:48.015285-06:00'
model: gpt-4-0125-preview
summary: "\u4EA4\u4E92\u5F0Fshell\uFF0C\u6216\u8BFB\u53D6-\u6C42\u503C-\u6253\u5370\
  \u5FAA\u73AF\uFF08REPL\uFF09\uFF0C\u8BA9\u4F60\u80FD\u591F\u8F93\u5165PowerShell\u547D\
  \u4EE4\u5E76\u7ACB\u5373\u83B7\u53D6\u53CD\u9988\u3002\u7A0B\u5E8F\u5458\u4F7F\u7528\
  \u5B83\u6765\u5FEB\u901F\u6D4B\u8BD5\u4EE3\u7801\u7247\u6BB5\u3001\u8C03\u8BD5\u6216\
  \u5728\u4E0D\u7F16\u5199\u5B8C\u6574\u811A\u672C\u7684\u60C5\u51B5\u4E0B\u5B66\u4E60\
  \u65B0\u547D\u4EE4\u3002."
title: "\u5728\u7F16\u7A0B\u4E2D\u4F7F\u7528\u4EA4\u4E92\u5F0FShell\uFF08REPL\uFF09"
weight: 34
---

## 什么和为什么？
交互式shell，或读取-求值-打印循环（REPL），让你能够输入PowerShell命令并立即获取反馈。程序员使用它来快速测试代码片段、调试或在不编写完整脚本的情况下学习新命令。

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
