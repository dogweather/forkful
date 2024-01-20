---
title:                "获取当前日期"
html_title:           "Arduino: 获取当前日期"
simple_title:         "获取当前日期"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/php/getting-the-current-date.md"
---

{{< edit_this_page >}}

# PHP 中的当前日期：为什么和怎么做？

## 什么 & 为什么？
获取当前日期是提取当前系统日期的过程。程序员会利用这个功能在编写程序时存储或显示特定时间，如记录事件发生的时间和生成详细的日志。

## 怎么做：
在 PHP 中，可以使用 `date()` 函数或 `DateTime` 类来获取当前日期。以下是一些示例：

使用 `date()` 函数：

```PHP
<?php
echo date('Y-m-d H:i:s');
?>
```

输出：

```PHP
2022-04-20 12:34:56
```

使用 `DateTime` 类：

```PHP
<?php
$date = new DateTime();
echo $date->format('Y-m-d H:i:s');
?>
```

输出：

```PHP
2022-04-20 12:34:56
```

## 深入理解
获取当前日期虽然看起来简单，但其实在底层，PHP 处理这个功能的过程却相当复杂。

首先，理解一下历史背景，在早期的 PHP 版本中，只能通过 `date()` 函数来获取日期，但在 PHP 5.2.0 中引入了 `DateTime` 类，提供了更多的功能并提高了处理日期和时间的灵活性。

另外，除了上述的方法之外，还有其他获取日期的方法，如 `getdate()`，该函数返回的是一个关联数组，包含了日期和时间的信息。

最后，关于 `date()` 和 `DateTime` 类的区别，

- `date()` 是一个函数，需要提供日期格式作为参数，并且它是依赖于时区设置的。
- `DateTime` 类提供了更多的方法和选项，可以处理日期和时间相关复杂的计算，如加减日期等。

## 参考信息
- PHP 官方文档 - date() 函数：https://www.php.net/manual/zh/function.date.php
- PHP 官方文档 - DateTime 类：https://www.php.net/manual/zh/class.datetime.php
- PHP 官方文档 - getdate() 函数：https://www.php.net/manual/zh/function.getdate.php