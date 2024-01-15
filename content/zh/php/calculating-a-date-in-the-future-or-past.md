---
title:                "在计算机编程中，这是一篇名为“计算未来或过去日期”的文章。"
html_title:           "PHP: 在计算机编程中，这是一篇名为“计算未来或过去日期”的文章。"
simple_title:         "在计算机编程中，这是一篇名为“计算未来或过去日期”的文章。"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/php/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# 为什么要计算未来或过去的日期

计算未来或过去的日期在编程中是一项常见的任务。它可以帮助程序员确定特定日期的准确值，如处理计划任务、生成报告或计算过时的文件。此外，它还有助于处理不同时区之间的日期转换。

## 如何做

使用PHP函数`strtotime()`和`date()`可以轻松地计算未来或过去的日期。首先，您需要提供一个日期字符串作为第一个参数，然后是要添加或减去的时间间隔以及可选的格式字符串作为第三个参数。

例如，这是一个计算未来日期的示例代码：

```PHP
$date = "2020-08-15";
$future_date = strtotime("+1 week", strtotime($date));
echo date("Y-m-d", $future_date);
```

运行上述代码的输出将是`2020-08-22`。同样，您也可以计算过去的日期，只需将时间间隔改为负值即可。

## 深入了解

在PHP中计算未来或过去的日期并不是一件复杂的事情。基本上，您需要掌握两个关键函数：`strtotime()`和`date()`。`strtotime()`函数将日期字符串转换为时间戳，在此基础上，您可以使用`date()`函数根据您的需要格式化日期。

然而，需要注意的是，`strtotime()`函数有其局限性，它只能处理某些日期格式。如果您的日期字符串没有格式正确，它将返回`false`。因此，在编写代码时要格外注意日期字符串的格式。

# 参考链接

- [PHP官方文档：strtotime()](https://www.php.net/manual/zh/function.strtotime.php)
- [PHP官方文档：date()](https://www.php.net/manual/zh/function.date.php)