---
title:                "获取当前日期"
html_title:           "Arduino: 获取当前日期"
simple_title:         "获取当前日期"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/powershell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 什么是获取当前日期与为何程序员需要获取当前日期？
获取当前日期是获取消耗电脑时间的过程。程序员是因为许多原因需要获取当前日期的：日志，任务调度，数据分析，和技术支持，等等。

## 如何获取当前日期：使用 PowerShell 示例
你可以使用 `Get-Date` 命令在 PowerShell 中获取当前日期。以下是使用案例：

```PowerShell
Get-Date
```

运行上述命令后的输出如下：

```PowerShell
Sunday, January 9, 2022 10:05:30 PM
```
## 深入研究
PowerShell 中获取当前日期的方式有着悠久的历史。自从 PowerShell 1.0 版本开始，微软就为190+个命令提供支持，其中包括 `Get-Date`。对于获取当前日期，我们还有其他替代的命令，如 `.NET` 里面的 `[DateTime]::Now`。不过，处理日期的实现细节，例如日期格式和时区，将会根据你的操作系统和语言设置而略有不同。

## 延伸阅读
1. Microsoft Docs, [Get-Date](https://docs.microsoft.com/zh-cn/powershell/module/microsoft.powershell.utility/get-date?view=powershell-7.1)
2. Stack Overflow, [How to get current datetime on Windows command line, in a suitable format for using in a filename?](https://stackoverflow.com/questions/203090/how-to-get-current-datetime-on-windows-command-line-in-a-suitable-format-for-us)