---
date: 2024-01-26 03:46:35.865150-07:00
description: "\u56DB\u820D\u4E94\u5165\u662F\u6307\u5C06\u6570\u503C\u8C03\u6574\u5230\
  \u6700\u63A5\u8FD1\u7684\u6574\u6570\u6216\u6307\u5B9A\u7684\u5C0F\u6570\u4F4D\u3002\
  \u7A0B\u5E8F\u5458\u8FDB\u884C\u56DB\u820D\u4E94\u5165\u4EE5\u7B80\u5316\u6570\u636E\
  \u3001\u63D0\u9AD8\u53EF\u8BFB\u6027\u6216\u5728\u8BA1\u7B97\u8FC7\u7A0B\u4E2D\u6EE1\
  \u8DB3\u67D0\u4E9B\u6570\u5B66\u8981\u6C42\u3002"
lastmod: '2024-02-25T18:49:45.573149-07:00'
model: gpt-4-0125-preview
summary: "\u56DB\u820D\u4E94\u5165\u662F\u6307\u5C06\u6570\u503C\u8C03\u6574\u5230\
  \u6700\u63A5\u8FD1\u7684\u6574\u6570\u6216\u6307\u5B9A\u7684\u5C0F\u6570\u4F4D\u3002\
  \u7A0B\u5E8F\u5458\u8FDB\u884C\u56DB\u820D\u4E94\u5165\u4EE5\u7B80\u5316\u6570\u636E\
  \u3001\u63D0\u9AD8\u53EF\u8BFB\u6027\u6216\u5728\u8BA1\u7B97\u8FC7\u7A0B\u4E2D\u6EE1\
  \u8DB3\u67D0\u4E9B\u6570\u5B66\u8981\u6C42\u3002"
title: "\u6570\u5B57\u53D6\u6574"
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
