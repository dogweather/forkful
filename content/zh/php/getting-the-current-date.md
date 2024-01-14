---
title:                "PHP: 获取当前日期"
programming_language: "PHP"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/php/getting-the-current-date.md"
---

{{< edit_this_page >}}

为什么：获取当前日期是编程中一个非常常见的任务。它可以帮助我们在网站或应用中显示准确的时间信息，也可以用于日志记录或数据分析。在本篇博文中，我们将学习如何使用PHP来获取当前日期。

如何：获取当前日期在PHP中非常简单。我们可以使用内置的date()函数来实现这个功能。让我们来看一个示例：

```PHP
<?php
// 获取当前日期
$currentDate = date("Y-m-d");

// 打印输出当前日期
echo "今天是：" . $currentDate;
```

以上代码将会输出类似于“今天是：2021-07-26”的信息。我们可以根据自己的需求，使用不同的日期格式来显示当前日期，例如月份、星期、小时等等。具体的日期格式参数可以参考PHP官方文档中的[date()函数](https://www.php.net/manual/en/function.date.php)部分。

深入解析：获取当前日期的原理是通过计算机的系统时间来实现的。当我们调用date()函数时，PHP会读取系统时间，并根据我们定义的格式来格式化输出。这一过程是自动完成的，所以不需要我们手动去获取系统时间。同时，PHP也提供了其他一些有用的日期和时间函数，比如获取时间戳、计算日期之间的差等等。如果想要深入了解日期和时间处理的相关知识，可以查看PHP官方文档中的[Date and Time相关章节](https://www.php.net/manual/en/datetime.php)。

另外，需要注意的是，系统时间受到时区的影响。在PHP中，我们可以使用`date_default_timezone_set()`函数来设置时区，以确保获取到正确的当前日期。更多时区相关信息可以参考[PHP官方文档中的时区列表](https://www.php.net/manual/en/timezones.php)。

同类文章：如果你对于PHP的日期和时间处理还想了解更多，可以阅读我们的其他博文[如何在PHP中格式化日期](https://www.example.php/blog/formatting-dates-in-php)及[如何计算日期差](https://www.example.php/blog/calculating-date-differences-in-php)。同时，也可以关注PHP官方文档中的相关内容。