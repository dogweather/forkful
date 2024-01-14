---
title:                "Arduino: 获取当前日期"
simple_title:         "获取当前日期"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 为什么
为了让你的代码与时间相关，你想要获取当前的日期是很有用的。比如说，你可能想要在某个特定的日期触发某个操作，或者在不同的日期显示不同的信息。

## 怎么做
```Arduino
// 首先，我们需要引入Time库
#include <Time.h>

void setup() {
  // 初始化串口通信，使我们能够在串口监视器中查看输出
  Serial.begin(9600);

  // 获取当前时间
  time_t now = time(nullptr);

  // 使用hour(), minute(), second()函数分别获取当前的小时、分钟和秒数
  int hour = hour(now);
  int minute = minute(now);
  int second = second(now);

  // 使用year(), month(), day()函数分别获取当前的年、月和日
  int year = year(now);
  int month = month(now);
  int day = day(now);

  // 输出日期和时间信息到串口监视器
  Serial.print("当前时间为：");
  Serial.print(hour);
  Serial.print(":");
  Serial.print(minute);
  Serial.print(":");
  Serial.print(second);
  Serial.print(", ");
  Serial.print(year);
  Serial.print("/");
  Serial.print(month);
  Serial.print("/");
  Serial.println(day);
}
```

代码解析：

- 首先，在`setup()`函数中，我们需要初始化串口通信，并将波特率设置为9600。
- 接着，我们通过调用`time()`函数来获取当前的时间，并将其赋值给`now`变量。请注意，`time()`函数来自于Time库，在使用之前需要先引入该库。
- 使用`hour()`, `minute()`和`second()`函数可以分别获取当前的小时、分钟和秒数，将它们赋值给对应的变量。
- 相同的，使用`year()`, `month()`和`day()`函数可以分别获取当前的年、月和日，将它们赋值给对应的变量。
- 最后，通过输出这些变量的值到串口监视器，我们可以获得当前的日期和时间信息。

输出示例：
```
当前时间为：12:34:56, 2020/7/15

## 深入了解
获取当前时间的方法并不仅限于使用Time库中的函数。事实上，在Arduino中有很多其他的方法可以获取当前日期和时间，比如通过连接WiFi获取NTP（网络时间协议）时间。你也可以使用实时时钟模块（RTC）来获取准确的时间信息。

## 另请参阅
- [介绍Time库](https://www.arduino.cc/en/Reference/Time): Arduino官方文档中关于Time库的介绍。
- [如何使用NTP时间](https://create.arduino.cc/projecthub/vancasts/nmbs-station-clock-25f97f?ref=search&ref_id=NTP&offset=2): 一个使用WiFi与NTP服务器来获取时间的项目示例。
- [使用实时时钟模块DS3231](https://www.instructables.com/id/Real-Time-Clock-With-Arduino-Using-DS1307-and-DS323/): 一个教你如何通过DS3231实时时钟模块获取时间信息的指南。