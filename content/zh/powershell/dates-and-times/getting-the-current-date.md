---
title:                "获取当前日期"
aliases: - /zh/powershell/getting-the-current-date.md
date:                  2024-02-03T19:10:24.034460-07:00
model:                 gpt-4-0125-preview
simple_title:         "获取当前日期"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/powershell/getting-the-current-date.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 什么与为什么？

在 PowerShell 中获取当前日期是指提取系统当前的日期和时间。这一操作对于记录日志、定时操作或基于日期做决定等任务至关重要。程序员利用这一能力来追踪事件、安排任务以及在脚本和应用程序中处理与日期相关的逻辑。

## 如何操作：

PowerShell 提供了直接的 cmdlet 用于获取日期和时间。`Get-Date` cmdlet 是此目的的主要工具。它可以返回完整的日期和时间，您可以根据需要对其进行格式化或操作。

```powershell
# 获取当前的日期和时间
Get-Date
```

**示例输出：**

```
2023年9月5日 星期二 上午9:46:02
```

您还可以格式化输出以仅显示您需要的信息，如仅日期或仅时间。

```powershell
# 以特定格式仅获取当前日期
Get-Date -Format "yyyy-MM-dd"
```

**示例输出：**

```
2023-09-05
```

```powershell
# 仅获取当前时间
Get-Date -Format "HH:mm:ss"
```

**示例输出：**

```
09:46:02
```

### 使用 .NET 类

PowerShell 允许直接访问 .NET 类，提供了另一种使用日期和时间的方式。

```powershell
# 使用 .NET DateTime 类获取当前的日期和时间
[System.DateTime]::Now
```

**示例输出：**

```
2023年9月5日 星期二 上午9:46:02
```

对于 UTC 时间：

```powershell
# 使用 .NET DateTime 类获取当前的 UTC 日期和时间
[System.DateTime]::UtcNow
```

**示例输出：**

```
2023年9月5日 星期二 下午1:46:02
```

这些命令和类为在 PowerShell 中处理日期和时间提供了强大且灵活的选项，对于许多脚本和自动化任务至关重要。
