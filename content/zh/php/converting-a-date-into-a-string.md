---
title:                "PHP: 将日期转换为字符串"
programming_language: "PHP"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/php/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# 为什么要将日期转换成字符串

日期格式在编程中是非常常见的。当我们需要在网站或应用中显示日期时，通常会将其转换成字符串。这样可以更容易阅读，也可以选择不同的显示格式。接下来，我们将介绍如何用PHP将日期转换成字符串，并深入了解这个过程。

## 如何操作

我们可以使用PHP内置的 ```date()``` 函数来将日期转换成字符串。下面是一个简单的示例：

```PHP
$date = date("Y-m-d");
echo $date;
```

在这个例子中，我们首先将今天的日期保存在变量 ```$date``` 中，然后使用 ```date()``` 函数来将其转换成字符串。在函数的参数中，我们指定了日期的格式，这里是年-月-日的格式。然后使用 ```echo``` 函数来显示字符串，这里的输出结果是：2020-11-17。

除了指定日期的格式，我们还可以在函数的第二个参数中传入具体的日期，如下所示：

```PHP
$date = date("l, F j, Y", strtotime("2021-01-01"));
echo $date;
```

这里我们在第一个参数中指定了日期的格式，包括星期几、月份和日期等信息。然后使用 ```strtotime()``` 函数将字符串日期转换成时间戳。最后输出的结果是：Friday, January 1, 2021。

## 深入了解

在PHP中，日期和时间被表示为时间戳，即从1970年1月1日00:00:00 UTC起经过的秒数。 ```date()``` 函数可以将时间戳转换成指定格式的日期字符串。

除了常用的格式如年-月-日和星期几、月份和日期等， ```date()``` 函数还有许多格式选项，如时钟格式、时区等。你可以在PHP官方文档中详细了解这些选项。

# 参考链接

- [PHP官方文档：date()函数](https://www.php.net/manual/zh/function.date.php)
- [PHP官方文档：strtotime()函数](https://www.php.net/manual/zh/function.strtotime.php)
- [PHP官方文档：日期和时间格式选项](https://www.php.net/manual/zh/datetime.formats.php)

# 参见

- [PHP官方文档：日期和时间](https://www.php.net/manual/zh/datetime.php)
- [PHP官方文档：日期和时间函数](https://www.php.net/manual/zh/ref.datetime.php)