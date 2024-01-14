---
title:                "PHP: 比较两个日期"
programming_language: "PHP"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/php/comparing-two-dates.md"
---

{{< edit_this_page >}}

＃＃ 为什么 

在编程中，经常会遇到需要比较两个日期的情况。比如在活动管理系统中，我们可能需要判断用户是否已经报名了过期的活动，或者在电子商务网站中，我们需要检查订单是否超过有效期。因此，了解如何比较两个日期是非常有用的技能。 

＃＃ 如何 

比较两个日期的方法并不复杂，只需要用到PHP的内置函数`strtotime()`和`date()`。以下是一个简单的例子： 

```PHP 
// 定义两个日期 
$date1 = "2021-01-01"; 
$date2 = "2021-02-01"; 

// 将日期转换成时间戳 
$timestamp1 = strtotime($date1); 
$timestamp2 = strtotime($date2); 

// 使用date()函数将时间戳转换成指定格式的日期 
$date1 = date("Y-m-d", $timestamp1); 
$date2 = date("Y-m-d", $timestamp2); 

// 比较两个日期的大小 
if ($timestamp1 < $timestamp2) { 
  echo "{$date1} 在 {$date2} 之前"; 
} else { 
  echo "{$date1} 在 {$date2} 之后"; 
} 

// 输出结果： 2021-01-01 在 2021-02-01 之前 
```

＃＃ 深入 

在深入了解比较两个日期之前，我们需要先了解什么是时间戳。时间戳是一个自1970年1月1日以来经过的秒数，并且在PHP中，时间戳以整数的形式表示。因此，通过比较时间戳，我们就可以判断出哪一个日期更早或更晚。 

另外，PHP也提供了其他方法来比较日期，如`DateTime`类和`strtotime()`函数的可选参数。你可以通过查阅PHP官方文档来学习更多关于比较日期的方法。 

＃＃ 参考资料 

- PHP官方文档：https://www.php.net/manual/en/function.strtotime.php 
- W3School中文网：https://www.w3school.com.cn/php/func_date_strtotime.asp 

＃＃ 相关阅读