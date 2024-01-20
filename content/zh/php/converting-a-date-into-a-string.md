---
title:                "将日期转换为字符串"
html_title:           "Bash: 将日期转换为字符串"
simple_title:         "将日期转换为字符串"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/php/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？
日期转字符串是个重要过程，它描述了将日期数据类型转换成字符串类型。这种转换的需求经常发生，因为日期格式不易处理或展示，然而字符串则用于输入、处理及输出等多种场景。

## 如何操作： 
下面简短代码展示了如何在 PHP 中把日期转化为字符串：

```PHP
<?php
$date = new DateTime('2023-04-27');
echo $date->format('Y-m-d');
?>
```
这段代码的输出会是：
```
2023-04-27
```
## 深入探讨
历史背景中，PHP 的 `DateTime` 类从 PHP5.2.0 版本就开始出现，这个类用于日期和时间的处理。在早期，如果需要在 PHP 中处理日期和时间，那就得用 `strtotime()` 和 `date()` 函数，但这两个函数的使用方式颇复杂。另外，你也可以用 `getdate()` 函数来把日期转换成字符串，不过这个函数的返回结果会是个数组，其中包含了日期的相关信息。

实施细节方面，DateTime 类的 `format()` 方法会返回一个格式化的日期字符串。方法中的参数决定了日期输出的形式，比如 "Y-m-d" 这个参数会把日期输出为 "年-月-日" 的形式。

## 更多资源
有关 PHP 及日期处理的函数，下面资源可能会有所帮助：
- PHP官方文档日期和时间处理部分： https://www.php.net/manual/en/book.datetime.php
- 有关 PHP DateTime 对象的 W3Schools 教程：https://www.w3schools.com/php/php_ref_date.asp