---
title:                "比较两个日期"
html_title:           "Clojure: 比较两个日期"
simple_title:         "比较两个日期"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 什么与为什么？（What & Why?）

日期比较就是检查两个日期谁比谁早或晚。程序员通常需要对时间进行排序，计划任务，或者判断一个操作是否超时。

## 如何做（How to:）
我们将使用`TimeLib.h`库来执行此操作。首先，我们需要声明两个日期。
```Arduino
#include <TimeLib.h>

time_t t1 = now();
delay(5000); // Delay for 5 seconds
time_t t2 = now();
```
接下来，我们可以比较这两个日期：
```Arduino
if(t1 > t2){
  // t1 is later than t2
} else if(t1 < t2) {
  // t1 is earlier than t2
} else {
  // t1 is the same as t2
}
```
## 深入研究（Deep Dive）

对于Arduino，时间和日期的处理始终是一项挑战性任务。Arduino没有内置的时间跟踪功能，所以需要借助`TimeLib.h`库。

不同的方法和库能让你以不同的方式处理日期。像`RTCLib`或者`DS1307RTC`能提供额外的功能，例如秒表，记忆功能，且能连上实时时钟模块以实现精确度更高的时间跟踪。

在比较日期时，日期实际上是被转成了格林威治标准时间（GMT）的秒数，我们可以将其视为长整型的数字。

## 另请参见（See Also）

如果你需要进一步研究，这里有一些资源：
- [`TimeLib.h`库文档](https://www.arduino.cc/reference/en/libraries/time/)
- [使用`RTCLib`库查看和设置时间和日期](https://learn.adafruit.com/ds1307-real-time-clock-breakout-board-kit/overview)