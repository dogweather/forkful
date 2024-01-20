---
title:                "获取当前日期"
date:                  2024-01-20T15:12:54.690352-07:00
html_title:           "Bash: 获取当前日期"
simple_title:         "获取当前日期"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why? 什么和为什么?
获取当前日期是读取当前日历时间的过程。 程序员需要日期来跟踪事件、记录数据、执行定时任务。

## How to 如何操作:
Arduino本身没有内建的时钟来得到日期。你需要一个外部的实时时钟(RTC)模块，比如DS3231。下面是如何使用它的示例。

```Arduino
#include <Wire.h>
#include <RTClib.h>

RTC_DS3231 rtc;

void setup() {
  Wire.begin();
  Serial.begin(9600);

  if (!rtc.begin()) {
    Serial.println("找不到RTC");
    while (1);
  }

  if (rtc.lostPower()) {
    Serial.println("RTC失电，需要设置时间！");
    // 当RTC失电时，使用下面的行来设置时间
    // rtc.adjust(DateTime(F(__DATE__), F(__TIME__)));
  }
}

void loop() {
  DateTime now = rtc.now();
  
  Serial.print(now.year(), DEC);
  Serial.print('/');
  Serial.print(now.month(), DEC);
  Serial.print('/');
  Serial.print(now.day(), DEC);
  Serial.print(" ");
  Serial.print(now.hour(), DEC);
  Serial.print(':');
  Serial.print(now.minute(), DEC);
  Serial.print(':');
  Serial.print(now.second(), DEC);
  Serial.println();
  
  delay(1000);
}
```

输出样例:
```
2023/9/17 15:45:32
```

## Deep Dive 深入了解:
最早的微控制器没有内建的时钟，因此需要外部硬件来跟踪时间。RTC模块，例如DS3231，通常使用一个小型的硬币型电池，可以在主电源断开时继续运行。除了DS3231，还有其他许多RTC模块可供选择，例如DS1307或者更高精度的模块。

RTC模块通过I2C总线与Arduino通信，这意味着只需要两个引脚（SDA和SCL）。RTClib库使得使用这些RTC模块更为简单，因为库封装了所有复杂的底层操作。

除了使用RTC模块，你还可以通过网络获取时间，如使用NTP(Network Time Protocol)客户端。但这将会需要互联网连接并且相对复杂。

## See Also 查看更多:
1. [RTClib库文档](https://github.com/adafruit/RTClib)
2. [DS3231产品手册](https://datasheets.maximintegrated.com/en/ds/DS3231.pdf)
3. [I2C通信教程](https://www.arduino.cc/en/Tutorial/LibraryExamples/MasterReader)