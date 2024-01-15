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

## 为什么

获取当前日期对于Arduino编程来说非常重要，因为它可以帮助我们记录重要的时间信息，比如传感器数据的读取时间、程序的运行时间等。

## 如何获取当前日期

要获取当前日期，我们需要使用内置函数`millis()`来获取当前程序运行的毫秒数，并将其转换为年、月、日等日期信息。下面是一个简单的代码示例：

```Arduino
unsigned long milliseconds = millis(); //获取当前运行的毫秒数
unsigned long seconds = milliseconds / 1000; //毫秒数转换为秒数
unsigned long minutes = seconds / 60; //秒数转换为分钟数
unsigned long hours = minutes / 60; //分钟数转换为小时数
unsigned long days = hours / 24; //小时数转换为天数

//计算当前的年、月、日
int currentYear = 1970 + days / 365; //根据天数计算年份，假设从1970年1月1日开始计算
int currentMonth = days % 365 / 30 + 1; //根据剩余天数计算月份，假设每月30天
int currentDay = days % 365 % 30 + 1; //根据剩余天数计算日期，假设每月30天
```

通过这样的计算过程，就可以得到当前的日期信息。但是需要注意的是，由于不同月份的天数不同，这样的计算方式并不是十分精确，仅供参考。

## 深入了解

要更加精确地获取当前日期，我们可以使用DS1307实时时钟模块，它可以提供准确的日期和时间信息。首先，我们需要将DS1307模块连接到Arduino板，然后可以使用[这个库](https://github.com/adafruit/RTClib)来读取日期和时间。下面是一个简单的代码示例：

```Arduino
#include "RTClib.h"

//定义DS1307模块的连接引脚
#define SDA A4
#define SCL A5

RTC_DS1307 rtc;

void setup() {
  Serial.begin(9600);
  //将DS1307模块连接到Arduino板
  Wire.begin();
  rtc.begin();
  //初始化DS1307模块，日期和时间可以在此处设置
  rtc.adjust(DateTime(2019, 6, 28, 10, 30, 0));
}

void loop() {
  //获取当前日期和时间
  DateTime now = rtc.now();

  //打印日期和时间
  Serial.print(now.year(), DEC);
  Serial.print('/');
  Serial.print(now.month(), DEC);
  Serial.print('/');
  Serial.print(now.day(), DEC);
  Serial.print(' ');
  Serial.print(now.hour(), DEC);
  Serial.print(':');
  Serial.print(now.minute(), DEC);
  Serial.print(':');
  Serial.print(now.second(), DEC);
  Serial.println();
}
```

通过这个库，我们可以直接从DS1307模块中读取准确的日期和时间信息，消除了使用`millis()`函数的不精确性。

## 参考链接

- `millis()`函数文档：[https://www.arduino.cc/reference/en/language/functions/time/millis/](https://www.arduino.cc/reference/en/language/functions/time/millis/)
- DS1307模块库：[https://github.com/adafruit/RTClib](https://github.com/adafruit/RTClib)