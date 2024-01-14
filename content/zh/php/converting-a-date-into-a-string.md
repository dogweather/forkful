---
title:                "PHP: 将日期转换为字符串"
simple_title:         "将日期转换为字符串"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/php/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# 为什么要将日期转换成字符串？

日期和时间在编程中经常被用到。但是，在某些情况下，我们需要将日期转换成字符串格式。这可能是因为要在网页上显示日期，或者为了方便读取和处理数据。无论是什么原因，将日期转换成字符串是一个非常常见的编程任务，让我们来学习如何做到这一点吧！

## 如何转换日期成字符串

要将日期转换成字符串，我们可以使用 PHP 中预先定义的函数 `date()`。该函数接受两个参数：日期格式和时间戳。时间戳是表示特定日期和时间的数字。让我们看看下面的例子：

```PHP
// 今天的日期
$date = time();
// 将日期格式设定为 "Y年m月d日"，如：2021年05月26日
echo date("Y年m月d日", $date);
```

输出结果为：2021年05月26日。上面的例子中，我们将时间戳设置为 `time()`，这会自动检索当前日期和时间。如果我们想要转换一个特定的日期，只需将时间戳参数换成对应的日期即可。

除了日期格式，我们还可以在 `date()` 函数中加入其他参数，来定制转换后的字符串。例如，我们可以添加时区参数，让日期和时间与我们所在的时区相匹配。让我们来看个例子：

```PHP
// 设置日期和时间的时区为中国标准时间
date_default_timezone_set('Asia/Shanghai');
// 定义一个时间戳，如：2021年05月26日早上九点
$date = mktime(9, 0, 0, 5, 26, 2021);
// 将日期格式设定为 "D, d M Y H:i:s T"，如：Wed, 26 May 2021 09:00:00 CST
echo date("D, d M Y H:i:s T", $date);
```

输出结果为：Wed, 26 May 2021 09:00:00 CST。我们可以看到，除了日期格式外，还加入了时区信息。

## 深入了解日期转换成字符串

PHP 中的 `date()` 函数其实是通过 `strftime()` 函数来实现的。但是，它提供了更简单的界面来格式化日期，并直接返回转换后的字符串。`strftime()` 函数则允许我们更灵活地定制日期字符串，包括选择不同语言的日期格式。

除了 `date()` 和 `strftime()`，PHP 还提供了另一个处理日期和时间的重要函数：`strtotime()`。它允许我们将字符串格式的日期和时间转换成时间戳，从而更方便地进行日期和时间的运算。

## 参考

- [PHP官方文档 - date() 函数](https://www.php.net/manual/zh/function.date.php)
- [PHP官方文档 - strftime() 函数](https://www.php.net/manual/zh/function.strftime.php)
- [PHP官方文档 - strtotime() 函数](https://www.php.net/manual/zh/function.strtotime.php)

# 参考资料

请参考以下资料来了解更多有关日期和时间的处理方法：

- [PHP日期和时间 - W3School](https://www.w3school.com.cn/php/php_date.asp)
- [PHP日期和时间 - 菜鸟教程](https://www.runoob.com/php/php-date-time.html)
- [PHP内置日期和时间函数列表 - PHP官方文档](https://www.php.net/manual/zh/ref.datetime.php)