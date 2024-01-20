---
title:                "计算未来或过去的日期"
html_title:           "PHP: 计算未来或过去的日期"
simple_title:         "计算未来或过去的日期"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/php/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？

计算未来或过去的日期是指确定在某一特定日期前后的某一天。程序员需要这样做来为业务逻辑或数据分析提供日期相关的支持。

## 如何做：

让我们通过一个简单的示例来看看如何使用PHP的 `DateTime`类和 `DateInterval`类来计算未来的日期。

```PHP
<?php

// 创建一个当前日期的 DateTime 对象
$dt = new DateTime();

// 创建一个 DateInterval 对象，这里为一个期限为30天的期间
$interval = new DateInterval('P30D');

// 使用 DateTime::add 方法来计算未来的日期
$dt->add($interval);

// 打印出未来的日期
echo $dt->format('Y-m-d');
?>
```

当我们运行这段代码，它将显示从当前日期开始的30天后的日期，例如：

```
2022-03-10
```

## 深入探索：

在开发的早期阶段，PHP中并没有 `DateTime` 和 `DateInterval` 类，这使得你必须手动进行日期的加减，或者使用 `strtotime()` 函数。随着时间的发展，PHP中加入了更多新的方法来处理日期，包括我们在示例中使用的 `DateTime` 和 `DateInterval` 类。

当然，除了 `DateTime` 和 `DateInterval` 类，你还可以使用其他的函数和库来计算不同的日期，例如 JavaScript 的 `Date` 类、Python 的 `datetime` 库等。在选择使用哪种方法时，你需要根据实际需求和使用的语言来决定。

## 参考链接：

- PHP 官方文档中关于 `DateTime` 类的详细介绍：https://www.php.net/manual/zh/class.datetime.php
- PHP 官方文档中关于 `DateInterval` 类的详细介绍：https://www.php.net/manual/zh/class.dateinterval.php
- Stack Overflow 上的相关讨论：https://stackoverflow.com/questions/1183927/date-subtraction-in-php