---
title:                "数字取整"
date:                  2024-01-26T03:46:35.865150-07:00
model:                 gpt-4-0125-preview
simple_title:         "数字取整"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/powershell/rounding-numbers.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？
四舍五入是指将数值调整到最接近的整数或指定的小数位。程序员进行四舍五入以简化数据、提高可读性或在计算过程中满足某些数学要求。

## 如何操作：
在PowerShell中，你有一些方便的cmdlet和方法用于四舍五入：

- 来自Math类的`Round()`方法
```PowerShell
[Math]::Round(15.68) # 四舍五入到16
```
- 指定小数位：
```PowerShell
[Math]::Round(15.684, 2) # 四舍五入到15.68
```
- `Ceiling()`和`Floor()`，用于总是向上或向下四舍五入：
```PowerShell
[Math]::Ceiling(15.2) # 向上四舍五入到16
[Math]::Floor(15.9) # 向下四舍五入到15
```

## 深入探讨
四舍五入并不是什么新鲜事物；它自古以来就存在，对于贸易、科学和计时非常有用。说到PowerShell，`[Math]::Round()`默认遵循“银行家舍入法”，其中0.5会四舍五入到最接近的偶数，以减少统计操作中的偏差。

你并不只是局限于使用`[Math]`方法。想要更多控制？查看`[System.Math]::Round(Number, Digits, MidpointRounding)`，在这里你可以设置如何处理中点：远离零或四舍五入到偶数（即银行家舍入法）。

另一个角度：`System.Globalization.CultureInfo`对象。它有助于处理国际数字时的特定于地区的格式化和四舍五入偏好。

## 另见
- Microsoft关于Math方法的官方文档：[链接](https://learn.microsoft.com/en-us/dotnet/api/system.math?view=net-7.0)
- .NET中的小数四舍五入细节：[链接](https://learn.microsoft.com/en-us/dotnet/api/system.midpointrounding?view=net-7.0)
- StackOverflow上关于四舍五入的讨论：[链接](https://stackoverflow.com/questions/tagged/rounding+powershell)