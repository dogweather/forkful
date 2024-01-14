---
title:    "Arduino: 比较两个日期"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 为什么要比较两个日期

在Arduino编程中，经常会遇到需要比较两个日期的情况。比如检查一个事件是否在特定日期范围内发生，或者比较两个数据点的时间戳。比较日期可以帮助我们更有效地处理数据和控制系统的运行，因此具有重要的意义。

## 如何进行比较

在Arduino中，我们可以使用`millis()`函数来获取自系统启动以来经过的毫秒数，从而实现日期的比较。下面是一个简单的例子，比较两个日期是否相同：

```Arduino
// 设置要比较的日期
int year1 = 2021;
int month1 = 3;
int day1 = 18;
int year2 = 2021;
int month2 = 3;
int day2 = 18;

// 使用millis()函数获取当前时间
long now = millis();

// 计算日期对应的毫秒数
long target1 = (((year1 - 1970) * 365 + (year1 - 1969) / 4) * 24 * 60 * 60 * 1000) + ((month1 - 1 + (day1 - 1) * 31) * 24 * 60 * 60 * 1000);
long target2 = (((year2 - 1970) * 365 + (year2 - 1969) / 4) * 24 * 60 * 60 * 1000) + ((month2 - 1 + (day2 - 1) * 31) * 24 * 60 * 60 * 1000);

// 比较日期是否相同
if (target1 == target2) {
  Serial.println("日期相同！");
} else {
  Serial.println("日期不同！");
}
```

运行这段代码，如果两个日期相同，会输出“日期相同！”的消息。

## 深入了解比较日期

在Arduino中，日期实际上是以毫秒数来表示的。使用`millis()`函数可以获取当前时间的毫秒数，而使用一些数学运算可以将日期转换成相应的毫秒数。因此，在比较两个日期时，实际上是在比较它们对应的毫秒数是否相等。此外，根据1年等于365天的假设，我们可以通过一些数学公式来计算出日期对应的毫秒数。

## 参考链接

- [Arduino官方文档 - millis()函数](https://www.arduino.cc/reference/zh/language/functions/time/millis/)
- [Wikipedia - Epoch (reference date)](https://en.wikipedia.org/wiki/Epoch_(reference_date))
- [Stack Overflow - Arduino: Comparing two dates](https://stackoverflow.com/questions/49953155/arduino-comparing-two-dates)