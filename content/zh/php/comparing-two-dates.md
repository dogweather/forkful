---
title:                "比较两个日期"
html_title:           "Clojure: 比较两个日期"
simple_title:         "比较两个日期"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/php/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？

比较两个日期是一个在日期和时间中确定两个特定实例之间的相对大小的过程。程序员做这个是为了对日程、事件或是在特定时间跨度内发生的其他任何东西进行排序或调度。

## 如何做：

PHP 提供了几种建立和比较日期的方式。我们最常用的工具是 DateTime 对象。

```PHP
<?php
    $date1 = new DateTime('2022-01-01');
    $date2 = new DateTime('2022-01-31');

    if ($date1 > $date2) {
        echo "Date1 是之后的日期";
    } else {
        echo "Date2 是之后的日期";
    }
?>
```

上述代码的输出将会是 "Date2 是之后的日期"。

## 深入了解

历史上，PHP 中日期的比较曾经给编程带来许多麻烦，因为比较两个日期意味着需要对每一个组成部分（年，月，日）分别进行比较。在 PHP 5.2.0 后，引入了 DateTime 对象，极大地简化了日期和时间的管理。

您也可以使用其他一些方法比如 `strtotime() `函数进行比较，但实际上，这些方法效率更低，也更容易产生错误。

```PHP
<?php
    $date1 = strtotime('2022-01-01');
    $date2 = strtotime('2022-01-31');

    if ($date1 > $date2) {
        echo "Date1 是之后的日期";
    } else {
        echo "Date2 是之后的日期";
    }
?>
```

这段代码执行的结果与前面一致，但它的执行效率低于使用 DateTime 对象的方式。

## 参考文献

PHP 官方文档为您提供了更多关于日期比较的信息。下述链接是一些您可能会感兴趣的主题：

- DateTime 对象：[点击这里](https://www.php.net/manual/en/book.datetime.php)
- strtotime 函数：[点击这里](https://www.php.net/manual/en/function.strtotime.php)