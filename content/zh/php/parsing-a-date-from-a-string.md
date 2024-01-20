---
title:                "从字符串解析日期"
html_title:           "C: 从字符串解析日期"
simple_title:         "从字符串解析日期"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/php/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## 什么 & 为什么?

字符串日期解析是对存储为字符串的日期数据进行解析，以获取日期信息。程序员之所以要做这样的操作，主要是为了在进行日期和时间运算，或者将日期信息以特定格式显示给用户时，能准确地使用这些数据。

## 怎么做:

以下是使用 PHP 解析字符串日期的示例:

```PHP
<?php
$dateStr = "2022-10-05";
$date = date_create_from_format('Y-m-d', $dateStr);
echo date_format($date, 'F d, Y');
?>
```

上述代码的输出将会是:

```PHP
October 05, 2022
```

在这个例子中，我们一开始有一个 `Y-m-d` 格式的字符串日期。然后，我们使用 `date_create_from_format` 函数来解析它，并创建一个 DateTime 对象。最后，我们使用 `date_format` 函数将日期格式化为 `F d, Y` 格式，并显示出来。

## 深度探讨：

从历史角度来看，PHP 的日期解析函数在很大程度上是受到 Unix 时间戳的影响。Unix 时间戳是一个表示从 1970 年 1 月 1 日 00:00:00 到现在所过去的秒数的整数，因此，PHP 也采用了这种方式来处理日期和时间。

至于解析日期，除了上述方法，我们还可以使用 PHP 的 `strtotime` 函数，直接将任何英文格式的日期文本解析为 Unix 时间戳：

```PHP
<?php
$dateStr = "5th October 2022";
$timestamp = strtotime($dateStr);
echo date('F d, Y', $timestamp);
?>
```

然而，需要注意的是，由于 `strtotime` 对于非英文日期格式的支持并不完全，对于一些特定的日期格式，还是推荐使用 `date_create_from_format` 进行解析。

## 参考资料：

对于日期解析以及日期和时间处理与计算的更多内容，您可以参考以下资源：

1. [PHP date_create_from_format 函数](https://www.php.net/manual/zh/datetime.createfromformat.php)
2. [PHP strtotime 函数](https://www.php.net/manual/zh/function.strtotime.php)
3. [PHP 时间日期处理](https://www.runoob.com/php/php-date.html)