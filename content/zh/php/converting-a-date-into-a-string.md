---
title:                "将日期转换为字符串"
html_title:           "PHP: 将日期转换为字符串"
simple_title:         "将日期转换为字符串"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/php/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 为什么

在编程中，我们经常需要把日期转换成字符串的形式来处理。这样可以更方便地在程序中使用和显示日期。PHP提供了简单的方法来实现这个转换，让我们一起来看看吧！

## 如何进行日期到字符串的转换

首先，我们需要定义一个日期变量，可以使用`date_create()`函数来创建一个日期对象，例如：

```PHP
$date = date_create('2021-01-01');
```

然后，我们使用`date_format()`函数来将日期转换成字符串，函数的第一个参数是日期对象，第二个参数是我们想要的日期格式。

```PHP
$string_date = date_format($date, 'Y-m-d');
```

最后，我们可以使用`echo`语句来输出转换后的字符串。完整的代码如下：

```PHP
// 定义日期变量并转换成字符串
$date = date_create('2021-01-01');
$string_date = date_format($date, 'Y-m-d');

// 输出结果
echo $string_date; // 2021-01-01
```

运行以上代码，我们可以得到转换后的字符串"2021-01-01"作为输出。

## 深入了解日期到字符串的转换

在`date_format()`函数中，我们可以使用不同的日期格式来转换日期，例如：

- `Y`：四位数字表示的年份
- `m`：两位数字表示的月份
- `d`：两位数字表示的日期
- `F`：完整的月份英文名称
- `l`：完整的星期英文名称

完整的日期格式列表可以参考 [PHP官方文档](https://www.php.net/manual/en/function.date.php)。

此外，`date_format()`函数还支持传入一个[时区](https://zh.wikipedia.org/zh-cn/%E6%97%B6%E5%8C%BA)参数，可以根据不同的时区来输出相应的日期格式。例如：

```PHP
$date_timezone = date_create('2021-01-01', timezone_open('Asia/Shanghai'));
$string_date = date_format($date_timezone, 'Y-m-d');
echo $string_date; // 2021-01-01
```

这样可以保证在不同的时区下，输出的日期格式始终是一致的。

## 参考链接

- [PHP官方文档：date_format()函数](https://www.php.net/manual/en/function.date.php)
- [维基百科：时区](https://zh.wikipedia.org/zh-cn/%E6%97%B6%E5%8C%BA)