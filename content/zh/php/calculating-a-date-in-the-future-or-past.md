---
title:                "在计算机编程中，计算未来或过去的日期。"
html_title:           "PHP: 在计算机编程中，计算未来或过去的日期。"
simple_title:         "在计算机编程中，计算未来或过去的日期。"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/php/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

北京时间日期轴？

最近看到有人在网上提到"计算未来或过去的日期"，想必很多人都会感到好奇。那么，这到底是什么事情呢？为什么程序员们会这样做呢？

## 什么 & 为什么？

计算未来或过去的日期，就是通过编程来计算某个特定日期之前或之后的日期。程序员们通常会需要这样的功能来安排任务、记录事件或计算时间差。

## 如何：

```php
// 计算明天的日期
echo date('Y-m-d', strtotime('+1 day'));

// 计算一周后的日期
echo date('Y-m-d', strtotime('+1 week'));

// 计算一小时后的时间
echo date('H:i:s', strtotime('+1 hour'));
```

运行上面的代码，你将获得类似于以下的输出：

```
2021-02-28
2021-03-07
12:00:00
```

## 深入探讨：

历史上，人们一直在需要计算日期，但是随着计算机的发展，这项工作不再需要手动计算，而是可以由计算机来完成。除了使用PHP提供的日期函数，还可以使用第三方库来实现日期计算的功能，例如Carbon。此外，对于一些特殊需求，程序员们也可以自行编写算法来计算日期。

## 查看更多：

- [PHP官方文档-日期和时间函数](https://www.php.net/manual/en/ref.datetime.php)
- [使用Carbon库计算日期](https://carbon.nesbot.com/docs/)
- [用PHP计算日期的另一种方法](https://www.geeksforgeeks.org/calculate-date-dates-php/)

Happy coding!