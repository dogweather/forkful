---
title:                "计算未来或过去的日期"
aliases:
- /zh/arduino/calculating-a-date-in-the-future-or-past/
date:                  2024-01-20T17:30:57.965414-07:00
model:                 gpt-4-1106-preview
simple_title:         "计算未来或过去的日期"

tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 什么和为什么？
计算未来或过去的日期就是推算出在指定日期之前或之后的确切日期。程序员这样做是为了追踪事件、设定提醒或计算时间差。

## 如何做：
```Arduino
#include <Wire.h>
#include <RTClib.h>

RTC_DS3231 rtc;

void setup() {
  Serial.begin(9600);
  if (!rtc.begin()) {
    Serial.println("Couldn't find RTC");
    while (1);
  }

  // 设置当前日期时间
  rtc.adjust(DateTime(F(__DATE__), F(__TIME__)));

  // 计算未来日期：10天之后
  DateTime now = rtc.now();
  DateTime futureDate = now + TimeSpan(10,0,0,0);
  Serial.print("未来日期: ");
  Serial.print(futureDate.year(), DEC);
  Serial.print('/');
  Serial.print(futureDate.month(), DEC);
  Serial.print('/');
  Serial.println(futureDate.day(), DEC);

  // 计算过去日期：10天前
  DateTime pastDate = now - TimeSpan(10,0,0,0);
  Serial.print("过去日期: ");
  Serial.print(pastDate.year(), DEC);
  Serial.print('/');
  Serial.print(pastDate.month(), DEC);
  Serial.print('/');
  Serial.println(pastDate.day(), DEC);
}

void loop() {
  // 此处不需要代码
}
```
输出样例：
```
未来日期：2023/4/21
过去日期：2023/4/1
```

## 深入了解
计算未来或过去的日期在编程里是个常见任务，以前采用的是更加手动的方法来计算。现在，使用像RTClib这样的库，简化了处理时间和日期的复杂性。除了RTClib，还有TimeLib和其他的库可用。实现这一功能的时候，要考虑闰年和不同月份天数的变化。对于Arduino，时间通常是由外部的实时时钟模块（如DS3231）提供准确时间。

## 参见链接
- RTClib库文档：https://github.com/adafruit/RTClib
- Arduino时间库 (TimeLib)：https://www.pjrc.com/teensy/td_libs_Time.html
- DS3231实时时钟模块说明：https://datasheets.maximintegrated.com/en/ds/DS3231.pdf
