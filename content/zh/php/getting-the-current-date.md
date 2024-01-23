---
title:                "获取当前日期"
date:                  2024-01-20T15:15:48.050643-07:00
html_title:           "Bash: 获取当前日期"
simple_title:         "获取当前日期"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/php/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why? 什么和为什么？
获取当前日期在编程中是指通过代码取得系统当前的日期时间。程序员经常需要这个功能，原因包括记录日志、用户交互或数据时间戳。

## How to: 怎么做？
```PHP
<?php
// 获取当前日期和时间
echo date('Y-m-d H:i:s');
?>
```

输出示例：
```
2023-03-28 14:45:12
```

```PHP
<?php
// 只获取当前日期
echo date('Y-m-d');
?>
```

输出示例：
```
2023-03-28
```

```PHP
<?php
// 获取其他时区的日期和时间
date_default_timezone_set('Asia/Shanghai');
echo date('Y-m-d H:i:s');
?>
```

输出示例：
```
2023-03-28 14:45:12
```

## Deep Dive 深入探索
PHP 自从1995年就开始提供日期时间功能。其中`date()`函数是用来获取当前日期和时间的最基本方式。程序员可通过修改`date()`函数的参数来定制输出格式。

除了`date()`函数，PHP 还有`DateTime`类提供更完整的日期时间功能。例如，可以处理时间区间和不同格式的日期时间。

默认情况下，`date()`函数使用服务器配置的时区。而`date_default_timezone_set()`函数允许改变脚本运行时的时区设置。

## See Also 另请参阅
- [PHP 官方日期时间文档](https://www.php.net/manual/en/book.datetime.php)
- [PHP `date()` 函数](https://www.php.net/manual/en/function.date.php)
- [PHP `DateTime` 类](https://www.php.net/manual/en/class.datetime.php)
- [PHP 时区设置](https://www.php.net/manual/en/timezones.php)
