---
title:                "PHP: 获取当前日期"
simple_title:         "获取当前日期"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/php/getting-the-current-date.md"
---

{{< edit_this_page >}}

# 为什么需要获取当前日期?

获取当前日期是编程中常见的需求，特别是在与时间相关的操作中。无论是记录用户注册时间，还是在网站上显示最新更新的日期，都需要获取当前日期。PHP提供了简单易用的函数来获取当前日期，让我们一起来学习如何实现吧！


## 如何实现获取当前日期?

使用PHP内置函数`date()`即可获取当前日期。该函数接受两个参数，第一个参数为日期格式，第二个参数可选，表示指定时间戳。下面的示例将演示如何获取当前日期，并以不同格式输出。

```PHP
$current_date = date("Y-m-d"); // 返回当前日期，格式为年-月-日，如2020-01-01
echo "当前日期为：" . $current_date; // 输出：当前日期为：2020-01-01

$current_date = date("l, F jS, Y"); // 返回当前日期的完整格式，如Monday, March 2nd, 2020
echo "今天是：" . $current_date; // 输出：今天是：Monday, March 2nd, 2020
```

以上示例只展示了部分日期格式，你可以根据自己的需求选择合适的格式。详情可参考PHP官方文档中的[日期和时间函数](https://www.php.net/manual/en/ref.datetime.php)部分。

## 深入了解获取当前日期

除了常用的`date()`函数外，PHP还提供了其他一些有用的函数来处理日期和时间。比如`time()`函数可以获取当前时间的时间戳，`strtotime()`函数可以将日期字符串转换为时间戳，`strtotime()`函数则可以根据指定的格式解析日期字符串。通过结合使用这些函数，可以实现更复杂的操作，例如计算两个日期之间的间隔。想要了解更多关于日期和时间处理的知识，可以参考[PHP官方文档](https://www.php.net/manual/en/book.datetime.php)。

# 参考链接

- [PHP官方文档-日期和时间函数](https://www.php.net/manual/en/ref.datetime.php)
- [PHP官方文档-日期和时间扩展](https://www.php.net/manual/en/book.datetime.php)
- [PHP中文网-时间和日期处理](https://www.php.cn/toutiao-408136.html)