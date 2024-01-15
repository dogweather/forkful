---
title:                "比较两个日期"
html_title:           "Arduino: 比较两个日期"
simple_title:         "比较两个日期"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 为什么

在日常生活中，我们经常需要比较不同的日期。比如说，我们想知道某个事件发生的时间是否在某个特定日期之后，或者计算两个日期之间的天数差等等。因此，学习如何在Arduino上编程比较日期是非常有用的。

## 如何实现

要在Arduino上比较两个日期，我们首先需要了解日期的格式。日期可以用数字表示，例如2020年8月31日可以写作20200831。接下来，我们可以使用Arduino的`>（大于）`和`<（小于）`运算符来比较两个日期，例如：

```
Arduino code block:
int date1 = 20201010;
int date2 = 20201111;

if (date1 > date2) {
  Serial.println("Date 1 is after Date 2.");
} else if (date1 < date2) {
  Serial.println("Date 2 is after Date 1.");
} else {
  Serial.println("The dates are the same.");
}
```

输出将是：“Date 1 is after Date 2.”，因为10月10日在11月11日之后。我们也可以通过使用`== （等于）`运算符来检查两个日期是否相同。

此外，我们可以使用Arduino的时间库（time.h）来比较日期和时间，这样可以更精确地比较。这需要我们先将日期和时间转换为时间戳，即从1970年1月1日至今的秒数。然后，我们可以使用`difftime（）`函数来计算两个日期之间的差值，例如：

```
Arduino code block:
#include <Time.h>
time_t date1 = makeTime(0, 0, 0, 10, 10, 2020);
time_t date2 = makeTime(0, 0, 0, 11, 11, 2020);

if (difftime(date1, date2) > 0) {
  Serial.println("Date 1 is after Date 2.");
} else if (difftime(date1, date2) < 0) {
  Serial.println("Date 2 is after Date 1.");
} else {
  Serial.println("The dates are the same.");
}
```

输出将是：“Date 1 is after Date 2.”，因为这两个日期相差31天。更多关于使用Arduino时间库的信息，请参考深入探讨部分。

## 深入了解

以上介绍的方法只是比较日期的基本方法，我们还可以根据具体需求进行更复杂的比较。比如，我们可以使用`time_t_to_tm（）`函数将时间戳转换为年、月、日等的具体值，从而可以更灵活地比较。

另外，我们也可以使用日期和时间的库（DateTime.h）来进行更多的操作，如计算日期之间的天数差、验证是否为闰年等等。

最后，记得在比较日期时，考虑到闰年的影响以及日期格式的一致性，避免出现错误的结果。

## 查看更多

- [Arduino 官方网站](https://www.arduino.cc/)
- [Arduino 时间库文档](https://www.arduino.cc/reference/en/libraries/time/)
- [DateTime.h文档](https://github.com/PaulStoffregen/DateTime)