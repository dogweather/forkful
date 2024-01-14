---
title:    "PHP: 未来或过去的日期计算"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/php/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# 为什么要计算未来或过去的日期？

在编程中，计算日期在某些情况下是非常有用的。例如，计算未来或过去的日期可以帮助我们制定日程安排或跟踪任务的完成时间。此外，我们也可以通过计算日期来处理生日、节假日等重要日期。

## 如何进行日期计算

要编写一个能够计算未来或过去的日期的PHP程序，我们需要了解日期相关的一些函数和概念。首先，我们需要使用date()函数来获取当前日期和时间。然后，我们可以使用strtotime()函数来将日期转换为Unix时间戳，这使得日期计算更加容易。最后，我们可以使用date()函数来格式化我们想要的日期输出。

下面是一个简单的代码示例，用于计算未来5天后的日期，并将日期格式化为“年-月-日”的形式：

```PHP
$current_date = date("Y-m-d");
$future_date = strtotime("+5 days", strtotime($current_date));
$formatted_date = date("Y-m-d", $future_date);

echo "未来5天的日期是：" . $formatted_date;
```

输出结果应该为：“未来5天的日期是：年-月-日”。

## 深入挖掘日期计算

日期计算涉及到许多概念，例如时区、夏令时等。在PHP中，我们可以使用date_default_timezone_set()函数来指定日期的时区，默认情况下是服务器的时区。此外，我们还可以使用date()函数的第二个参数来指定日期的格式，例如“j”表示日， “m”表示月， “Y”表示年，等等。

当涉及到夏令时时，我们需要使用date("I")来检查给定的日期是否在夏令时期间。如果是，则需要将夏令时的偏移（通常为1小时）加到结果中。

## 参考资料

- PHP日期和时间函数手册：http://php.net/manual/en/book.datetime.php
- PHP时区列表：http://php.net/manual/en/timezones.php

# 参考资料

- PHP日期和时间函数手册：http://php.net/manual/en/book.datetime.php
- PHP时区列表：http://php.net/manual/en/timezones.php