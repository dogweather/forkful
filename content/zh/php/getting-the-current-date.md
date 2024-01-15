---
title:                "获取当前日期。"
html_title:           "PHP: 获取当前日期。"
simple_title:         "获取当前日期。"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/php/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 为什么

获取当前日期是编写 PHP 程序中常见的任务，因为日期和时间在许多 Web 应用程序中都是必不可少的。通过获取当前日期，开发人员可以在他们的应用程序中显示正确的日期和时间，为用户提供更好的体验。

## 如何

```PHP
<?php
     // 通过 date() 函数获取当前日期和时间
     $current_date = date('Y-m-d H:i:s');
     
     // 打印输出当前日期和时间
     echo "当前日期和时间是：" . $current_date;

     // 输出：当前日期和时间是：2019-09-27 14:25:45
?>
```

## 深入探讨

PHP 中有多种获取当前日期的方法，其中一种是使用 `date()` 函数。它接受两个参数，第一个参数是日期格式，第二个参数是可选的时间戳。日期格式定义了您希望返回日期的形式，如年、月、日、时、分、秒等。时间戳是一个可选参数，表示要返回日期的特定时间。如果没有提供时间戳，`date()` 函数将返回当前日期和时间。

除了 `date()` 函数，PHP 还提供了许多其他处理日期和时间的函数，如 `time()`、`strtotime()` 和 `strtotime()`。掌握这些函数将有助于您在开发过程中更灵活地处理日期和时间。

## 参考资料

- [PHP date() 函数](https://www.runoob.com/php/func-date.html)
- [PHP 时间和日期处理](https://www.php.net/manual/zh/datetime.examples-formats.php)
- [PHP 时间和日期函数指南](https://www.php.net/manual/zh/ref.datetime.php)