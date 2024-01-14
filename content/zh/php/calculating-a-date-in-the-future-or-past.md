---
title:                "PHP: 在未来或过去计算日期"
simple_title:         "在未来或过去计算日期"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/php/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# 为什么

计算未来或过去的日期有很多实际应用。无论是在日常生活中安排活动，还是在构建网站时，都可能需要计算日期。因此，学习如何使用PHP编程来计算日期可以为你省时省力。

# 如何做

在PHP中，有两个主要函数可以用来计算日期：date()和strtotime()。首先，使用date()函数来获取当前日期，然后使用strtotime()函数来计算未来或过去的日期。下面是两个示例：

```
<?php
// 获取当前日期
$currentDate = date("Y-m-d");

// 计算3天后的日期
$futureDate = strtotime("+3 days", strtotime($currentDate));

// 输出3天后的日期
echo date("Y-m-d", $futureDate);  // 输出：2021-07-21
```

```
<?php
// 获取当前日期
$currentDate = date("Y-m-d");

// 计算2周前的日期
$pastDate = strtotime("-2 weeks", strtotime($currentDate));

// 输出2周前的日期
echo date("Y-m-d", $pastDate);  // 输出：2021-07-01
```

通过改变时间单位以及日期格式，你可以轻松地计算任何未来或过去的日期。

# 深入探讨

除了基本的日期计算，PHP还提供了其他强大的功能来处理日期。比如，你可以使用mktime()函数来创建一个具有指定日期的Unix时间戳，然后再使用date()函数来将Unix时间戳转换为日期格式。此外，PHP还提供了一些日期格式化选项，如获取一周的第几天或一年中的第几个月份。这些功能可以帮助你更精确地计算和处理日期。

# 参考资料

- PHP官方文档：https://www.php.net/manual/zh/
- date()函数：https://www.php.net/manual/zh/function.date.php
- strtotime()函数：https://www.php.net/manual/zh/function.strtotime.php