---
title:                "计算未来或过去的日期"
html_title:           "PowerShell: 计算未来或过去的日期"
simple_title:         "计算未来或过去的日期"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/powershell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 什么以及为什么？
计算未来或过去的日期，这是在你确定一个日期之后或之前的特定周期的程序。这对于编程来说，非常重要，当你需要处理比如说追踪项目到期日期或日志文件的创建和修改日期等。

## 如何操作：
以下是 PowerShell代码以及示例的一部分，我们会使用到 'AddDays'这个函数来计算未来的日期。同样的，我们也会用到这个函数来计算过去的日期，只不过在参数中我们使用负数。

```PowerShell
# 计算未来的日期
$futureDate = (Get-Date).AddDays(10)
Write-Output "未来十天后的日期是: $futureDate"

# 计算过去的日期
$pastDate = (Get-Date).AddDays(-10)
Write-Output "过去十天前的日期是: $pastDate"
```

上述代码的输出为：
```PowerShell
未来十天后的日期是: 11/20/2022 2:41:59 PM
过去十天前的日期是: 11/10/2022 2:41:59 PM
```

## 深入探讨：
计算未来或过去的日期是单位时间运算的一部分，可以追溯到初级编程。 在类似的环境中，我们可以使用'.AddHours', '.AddMinutes', 或 '.AddSeconds' 和其他相关函数。

计算未来或过去的日期在各个编程语言中都有其等价的函数。比如说，在 Python 中，你可以使用 datetime 和 timedelta 函数；在 Java 中，你可以使用 Calendar 和 SimpleDateFormate 类。

PowerShell 中 'AddDays' 函数的实现方式如下，其中参数表示添加到当前日期的天数：
```PowerShell
public DateTime AddDays(double value);
```

## 另请参阅：
以下是一些相关参考链接: