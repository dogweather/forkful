---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:10:24.034460-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A PowerShell \u63D0\u4F9B\u4E86\u76F4\u63A5\
  \u7684 cmdlet \u7528\u4E8E\u83B7\u53D6\u65E5\u671F\u548C\u65F6\u95F4\u3002`Get-Date`\
  \ cmdlet \u662F\u6B64\u76EE\u7684\u7684\u4E3B\u8981\u5DE5\u5177\u3002\u5B83\u53EF\
  \u4EE5\u8FD4\u56DE\u5B8C\u6574\u7684\u65E5\u671F\u548C\u65F6\u95F4\uFF0C\u60A8\u53EF\
  \u4EE5\u6839\u636E\u9700\u8981\u5BF9\u5176\u8FDB\u884C\u683C\u5F0F\u5316\u6216\u64CD\
  \u4F5C\u3002"
lastmod: '2024-03-13T22:44:48.025981-06:00'
model: gpt-4-0125-preview
summary: "PowerShell \u63D0\u4F9B\u4E86\u76F4\u63A5\u7684 cmdlet \u7528\u4E8E\u83B7\
  \u53D6\u65E5\u671F\u548C\u65F6\u95F4\u3002`Get-Date` cmdlet \u662F\u6B64\u76EE\u7684\
  \u7684\u4E3B\u8981\u5DE5\u5177\u3002\u5B83\u53EF\u4EE5\u8FD4\u56DE\u5B8C\u6574\u7684\
  \u65E5\u671F\u548C\u65F6\u95F4\uFF0C\u60A8\u53EF\u4EE5\u6839\u636E\u9700\u8981\u5BF9\
  \u5176\u8FDB\u884C\u683C\u5F0F\u5316\u6216\u64CD\u4F5C."
title: "\u83B7\u53D6\u5F53\u524D\u65E5\u671F"
weight: 29
---

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
