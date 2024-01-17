---
title:                "获取当前日期"
html_title:           "PowerShell: 获取当前日期"
simple_title:         "获取当前日期"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/powershell/getting-the-current-date.md"
---

{{< edit_this_page >}}

# 现在时间是什么 & 为什么要获取它？
获取当前日期就是你想要知道当天是几月几号的日期。程序员经常这样做是因为它是一种固定操作，可以在编写代码时将它用作参考。

# 如何：
使用PowerShell获取当前日期很简单，只需要使用“Get-Date”命令即可。以下是一个示例代码和输出：

```PowerShell
Get-Date
```
输出：
> 2021年9月19日 17:13:03


你也可以使用参数来指定日期格式，如下所示：

```PowerShell
Get-Date -Format "yyyy-MM-dd"
```
输出：
> 2021-09-19

# 深入探讨：
从历史的角度来看，获取当前日期是非常重要的。在传统的编程中，获取当前日期需要编写较长且复杂的代码，而使用PowerShell则简单方便得多。另外，除了使用“Get-Date”命令外，你也可以通过调用.NET框架中的DateTime类来获取日期。

# 另请参阅：
- [Microsoft官方文档：Get-Date命令](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/get-date)
- [Wikipedia：“获取当前日期”](https://en.wikipedia.org/wiki/System_time)