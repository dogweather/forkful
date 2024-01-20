---
title:                "获取当前日期"
html_title:           "Arduino: 获取当前日期"
simple_title:         "获取当前日期"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 什么和为什么？
获取当前日期是一种获取编程时的实时日期的方法。程序员这么做是为了跟踪和记录程序运行和操作的实时数据。

## 如何做：
获取Arduino的当前日期很简单。

```Arduino
#include <RTClib.h>

RTC_DS1307 rtc;

void setup() {
  Serial.begin(9600);

  if (! rtc.begin()) {
    Serial.println("Couldn't find RTC");
    while (1);
  }

  if (! rtc.isrunning()) {
    Serial.println("RTC is NOT running!");
    // following line sets the RTC to the date & time this sketch was compiled
    rtc.adjust(DateTime(F(__DATE__), F(__TIME__)));
  }
}

void loop() {
  DateTime now = rtc.now();

  Serial.print(now.year(), DEC);
  Serial.print('/');
  Serial.print(now.month(), DEC);
  Serial.print('/');
  Serial.print(now.day(), DEC);
  Serial.println();
}
```
输出：
```Arduino
2021/7/19
```

## 深入探究
获取当前日期的流程起源于早期的编程，旨在帮助程序员更好地了解他们程序的运行状态。这被广泛应用于系统日志和实时事件跟踪。

与该方法的一个主要的替代方法是使用系统时间库（例如time.h或sys/time.h）。不过，获取 Arduino 的当前日期会更方便，因为它直接与板上的实时时钟（RTC）集成在一起。

在实现细节方面，本方法使用了 RTClib 库， 它是 Arduino 的一个实时时钟库。

## 另请参阅
了解 RTClib 的更多信息，可访问[这个链接](https://www.arduino.cc/reference/en/libraries/rtclib/)。
使用不同 Arduino 板的日期和时间的其它方法可以在[这个链接](https://create.arduino.cc/projecthub/Arduino_Scuola/date-and-time-using-only-the-arduino-uno-board-6c6f4d)看到。