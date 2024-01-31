---
title:                "获取当前日期"
date:                  2024-01-20T15:16:06.757792-07:00
simple_title:         "获取当前日期"

tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/powershell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why? 什么和为什么？
获取当前日期是指在PowerShell脚本中得到现在的日期和时间。程序员这么做是为了记录事件、处理定时任务或者仅仅是为了显示。

## How to: 怎么做？
```PowerShell
# 获取当前日期和时间
Get-Date

# 显示格式化的日期
Get-Date -Format "yyyy/MM/dd HH:mm:ss"

# 仅显示日期部分
(Get-Date).ToShortDateString()

# 仅显示时间部分
(Get-Date).ToShortTimeString()
```

样本输出：
```PowerShell
星期二, 23 三月, 2023 14:20:36
2023/03/23 14:20:36
2023/03/23
14:20
```

## Deep Dive 深入探索
PowerShell使用的Get-Date命令来自.NET的DateTime类，在2006年随着首个版本的PowerShell一起问世。你还可以使用.NET类直接来获取当前日期。相比于其他语言的日期处理，PowerShell提供了一种更直观且易于理解的方式来操作日期和时间。

历史上，Shell脚本比如Bash需要调用外部程序如`date`来获取日期。PowerShell简化了流程，让日期和时间的获取与格式化变得简单。

实现细节上，PowerShell的Get-Date思路是基于系统时钟和时区来显示日期。当你需要更加复杂的日期运算时，可以利用DateTime对象提供的方法和属性。

## See Also 参见
- [PowerShell Get-Date documentation](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/get-date?view=powershell-7.1)
- [Custom date and time format strings](https://docs.microsoft.com/en-us/dotnet/standard/base-types/custom-date-and-time-format-strings)
- [.NET DateTime Structure](https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=net-5.0)
