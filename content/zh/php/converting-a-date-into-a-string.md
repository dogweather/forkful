---
title:    "PHP: 将日期转换为字符串"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/php/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 为什么要将日期转换成字符串

在编程中，我们经常需要将日期转换成字符串的格式。这有助于我们将日期显示为想要的样式，也方便我们在处理日期数据时进行格式化。下面将介绍如何使用PHP将日期转换成字符串，并进行更深入的探讨。

## 如何实现日期转换成字符串

使用PHP，我们可以使用date()函数将日期转换成字符串的格式。下面给出一个简单的例子：

```PHP
$date = date("Y-m-d");
echo $date;
```

这段代码会将当前日期的年、月、日以“年-月-日”的形式输出。输出结果如下：

```shell
2021-10-11
```

我们也可以通过改变date()函数的参数来控制输出的日期格式，如下所示：

```PHP
$date = date("l, F jS, Y");
echo $date;
```

这将输出星期几加上月份和日期的形式，如下：

```shell
Monday, October 11th, 2021
```

其他可用的日期格式可以在PHP的官方文档中找到。

## 深入了解日期转换成字符串

除了使用date()函数，我们还可以使用strftime()函数来自定义日期的输出格式。这个函数接受两个参数，第一个是想要的日期格式，第二个是想要转换的日期。下面是一个例子：

```PHP
$date = strftime("%A, %B %d, %Y", strtotime("tomorrow"));
echo $date;
```

这将把明天的日期以“星期几, 月份 日, 年”的格式输出，如下所示：

```shell
Tuesday, October 12, 2021
```

使用strftime()函数可以更灵活地控制日期的显示格式，例如可以显示中文、英文缩写等。

## 参考资料

- [PHP官方文档-日期和时间](https://www.php.net/manual/en/datetime.format.php)
- [W3School-PHP date() 函数](https://www.w3schools.com/php/func_date_date.asp)
- [菜鸟教程-PHP strftime() 函数](https://www.runoob.com/php/func-date-strftime.html)

## 参见

- [PHP日期时间格式化指南](https://www.runoob.com/php/php-date-time.html)
- [如何在PHP中比较日期](https://www.runoob.com/php/php-date-compare.html)
- [PHP日期时间函数大全](https://www.runoob.com/php/php-date-time.html)