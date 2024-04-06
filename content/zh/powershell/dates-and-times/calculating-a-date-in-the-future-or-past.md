---
date: 2024-01-20 17:31:38.035146-07:00
description: "\u5982\u4F55\u505A\uFF1A \u5728 PowerShell \u4E2D\uFF0C`Get-Date` \u547D\
  \u4EE4\u7528\u6765\u83B7\u53D6\u5F53\u524D\u65E5\u671F\u548C\u65F6\u95F4\u3002`.AddDays()`\
  \ \u65B9\u6CD5\u5219\u662F\u57FA\u4E8E\u5F53\u524D\u65E5\u671F\u6DFB\u52A0\u6216\
  \u51CF\u5C11\u5929\u6570\u3002\u4EBA\u4EEC\u65E9\u5728\u8BA1\u7B97\u673A\u51FA\u73B0\
  \u4E4B\u524D\u5C31\u5DF2\u7ECF\u5728\u5904\u7406\u65E5\u671F\u95EE\u9898\u4E86\u3002\
  \u4E0D\u8FC7\uFF0C\u8BA1\u7B97\u673A\u8BA9\u8FD9\u4E2A\u8FC7\u7A0B\u81EA\u52A8\u5316\
  \u548C\u7CBE\u786E\u5316\u3002 \u9664\u4E86 PowerShell, \u5176\u5B83\u7F16\u7A0B\
  \u8BED\u8A00\uFF0C\u5982 Python \u7528\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T22:38:47.185475-06:00'
model: gpt-4-1106-preview
summary: "\u9664\u4E86 PowerShell, \u5176\u5B83\u7F16\u7A0B\u8BED\u8A00\uFF0C\u5982\
  \ Python \u7528 `datetime` \u6A21\u5757\uFF0CJavaScript \u7528 `Date` \u5BF9\u8C61\
  \uFF0C\u90FD\u62E5\u6709\u5904\u7406\u65E5\u671F\u7684\u80FD\u529B\u3002\u9274\u4E8E\
  \u4E0D\u540C\u7CFB\u7EDF\u53EF\u80FD\u4F1A\u56E0\u65F6\u533A\u548C\u65E5\u5149\u8282\
  \u7EA6\u65F6\u95F4\u7B49\u5BFC\u81F4\u65E5\u671F\u8BA1\u7B97\u590D\u6742\u5316\uFF0C\
  \u91CD\u8981\u7684\u662F\u4E86\u89E3\u4F60\u6240\u7528\u7684\u5E93\u6216\u8005\u529F\
  \u80FD\u5982\u4F55\u5904\u7406\u8FD9\u4E9B\u60C5\u51B5\u3002"
title: "\u8BA1\u7B97\u672A\u6765\u6216\u8FC7\u53BB\u7684\u65E5\u671F"
weight: 26
---

## 如何做：
```PowerShell
# 添加天数来计算未来日期
$futureDate = (Get-Date).AddDays(10)
Write-Output $futureDate

# 减少天数来计算过去日期
$pastDate = (Get-Date).AddDays(-10)
Write-Output $pastDate
```
输出样例：
```
星期五, 7 五月 2023 16:44:12
星期二, 17 四月 2023 16:44:12
```

## 深入了解
在 PowerShell 中，`Get-Date` 命令用来获取当前日期和时间。`.AddDays()` 方法则是基于当前日期添加或减少天数。人们早在计算机出现之前就已经在处理日期问题了。不过，计算机让这个过程自动化和精确化。

除了 PowerShell, 其它编程语言，如 Python 用 `datetime` 模块，JavaScript 用 `Date` 对象，都拥有处理日期的能力。鉴于不同系统可能会因时区和日光节约时间等导致日期计算复杂化，重要的是了解你所用的库或者功能如何处理这些情况。

历史上，过去时间的计算对于天文学和农业至关重要，而未来时间的计算则有助于规划和战略。在计算机编程中，准确计算日期对于任务调度、预测算法和时间序列分析都非常关键。

## 参见
- [PowerShell 官方文档 `Get-Date`](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/get-date?view=powershell-7.2)
