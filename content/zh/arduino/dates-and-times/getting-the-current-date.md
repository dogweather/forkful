---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:08:46.106528-07:00
description: "\u5728Arduino\u9879\u76EE\u4E2D\u83B7\u53D6\u5F53\u524D\u65E5\u671F\u6D89\
  \u53CA\u83B7\u53D6\u5B9E\u65F6\u4FE1\u606F\uFF0C\u8FD9\u5BF9\u4E8E\u65E5\u5FD7\u8BB0\
  \u5F55\u3001\u65F6\u95F4\u6233\u6216\u5B89\u6392\u4EFB\u52A1\u81F3\u5173\u91CD\u8981\
  \u3002\u7A0B\u5E8F\u5458\u901A\u5E38\u9700\u8981\u8FD9\u4E2A\u529F\u80FD\u4EE5\u589E\
  \u5F3A\u529F\u80FD\u6027\uFF0C\u786E\u4FDD\u6570\u636E\u7684\u76F8\u5173\u6027\uFF0C\
  \u5E76\u5728\u4ED6\u4EEC\u7684IoT\u548C\u5D4C\u5165\u5F0F\u9879\u76EE\u4E2D\u4FC3\
  \u8FDB\u4E0E\u65F6\u95F4\u654F\u611F\u7684\u64CD\u4F5C\u3002"
lastmod: '2024-03-13T22:44:48.074256-06:00'
model: gpt-4-0125-preview
summary: "\u5728Arduino\u9879\u76EE\u4E2D\u83B7\u53D6\u5F53\u524D\u65E5\u671F\u6D89\
  \u53CA\u83B7\u53D6\u5B9E\u65F6\u4FE1\u606F\uFF0C\u8FD9\u5BF9\u4E8E\u65E5\u5FD7\u8BB0\
  \u5F55\u3001\u65F6\u95F4\u6233\u6216\u5B89\u6392\u4EFB\u52A1\u81F3\u5173\u91CD\u8981\
  \u3002\u7A0B\u5E8F\u5458\u901A\u5E38\u9700\u8981\u8FD9\u4E2A\u529F\u80FD\u4EE5\u589E\
  \u5F3A\u529F\u80FD\u6027\uFF0C\u786E\u4FDD\u6570\u636E\u7684\u76F8\u5173\u6027\uFF0C\
  \u5E76\u5728\u4ED6\u4EEC\u7684IoT\u548C\u5D4C\u5165\u5F0F\u9879\u76EE\u4E2D\u4FC3\
  \u8FDB\u4E0E\u65F6\u95F4\u654F\u611F\u7684\u64CD\u4F5C\u3002."
title: "\u83B7\u53D6\u5F53\u524D\u65E5\u671F"
weight: 29
---

## 什么 & 为什么？
在Arduino项目中获取当前日期涉及获取实时信息，这对于日志记录、时间戳或安排任务至关重要。程序员通常需要这个功能以增强功能性，确保数据的相关性，并在他们的IoT和嵌入式项目中促进与时间敏感的操作。

## 如何实现：
Arduino本身没有内置的方法可以直接获取当前日期，因为它缺少实时时钟（RTC）。然而，可以使用外部RTC模块（如DS3231）和库（例如Adafruit开发的`RTClib`），这使得与这些模块的接口变得简单。

首先，确保`RTClib`库安装在你的Arduino IDE中。然后，根据其文档将你的RTC模块连接到Arduino。

以下是一个简单的示例，帮助你开始：

```cpp
#include <Wire.h>
#include "RTClib.h"

RTC_DS3231 rtc;

void setup() {
  Serial.begin(9600);

  if (!rtc.begin()) {
    Serial.println("找不到RTC");
    while (1);
  }

  if (rtc.lostPower()) {
    Serial.println("RTC失去电源，让我们设置时间！");
    // 当需要在新设备上设置时间或电源丢失后，你可以在这里设置时间。
    // rtc.adjust(DateTime(F(__DATE__), F(__TIME__)));
  }
}

void loop() {
  DateTime now = rtc.now();

  Serial.print("当前日期：");
  Serial.print(now.year(), DEC);
  Serial.print('/');
  Serial.print(now.month(), DEC);
  Serial.print('/');
  Serial.println(now.day(), DEC);

  delay(3000); // 延迟3秒以减少串行垃圾
}
```

示例输出（假设你的RTC之前已设置）：

```
当前日期：2023/4/15
```

该代码初始化RTC模块，然后，在循环中，每3秒将当前日期获取并打印到串行监视器一次。请记住，`rtc.adjust(...)`这一行可以取消注释并修改，以最初或在其失去电力后设置RTC的日期和时间。
