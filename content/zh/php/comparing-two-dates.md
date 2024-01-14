---
title:    "PHP: 比较两个日期"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/php/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 为什么比较两个日期？

在编写一个复杂的网站或应用程序时，经常会遇到需要比较日期的情况。比如，检查用户的账户是否过期或者判断某个事件是否已经发生。比较两个日期可以帮助我们轻松实现这些功能。

## 如何进行日期比较？

比较日期通常会涉及到使用一些内置的PHP函数。首先，我们需要将两个日期都转换成时间戳，然后可以使用`if`语句来比较它们。下面是一个简单的例子：

```PHP
$first_date = strtotime("2020-01-01");
$second_date = strtotime("2020-02-01");

if($first_date < $second_date){
  echo "第一个日期早于第二个日期";
}else{
  echo "第一个日期晚于或等于第二个日期";
}
```

上面的代码会输出`第一个日期早于第二个日期`，因为2020年1月1日早于2020年2月1日。

除了比较日期大小，我们还可以使用`date_diff()`函数来计算两个日期之间的差距，以便更精确地比较。

## 深入了解日期比较

在PHP中，时间戳是一个整数表示从1970年1月1日以来的秒数。这就是为什么我们需要在比较日期之前将它们转换成时间戳。当日期被转换成时间戳后，我们可以使用各种数学运算符来比较它们。

此外，我们也可以使用`strtotime()`函数来将日期字符串转换成时间戳，这样就可以方便地在比较中使用。

## 参考资料

- [PHP官方文档：日期和时间](https://www.php.net/manual/zh/book.datetime.php)
- [W3School教程：PHP日期与时间](https://www.w3school.com.cn/php/php_date.asp)
- [菜鸟教程：PHP日期和时间](https://www.runoob.com/php/php-date-time.html)

# 参见

- [PHP官方文档：时间戳](https://www.php.net/manual/zh/function.time.php)
- [PHP官方文档：日期比较](https://www.php.net/manual/zh/function.date-compare.php)