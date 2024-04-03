---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:08:46.106528-07:00
description: "\u5982\u4F55\u5B9E\u73B0\uFF1A Arduino\u672C\u8EAB\u6CA1\u6709\u5185\
  \u7F6E\u7684\u65B9\u6CD5\u53EF\u4EE5\u76F4\u63A5\u83B7\u53D6\u5F53\u524D\u65E5\u671F\
  \uFF0C\u56E0\u4E3A\u5B83\u7F3A\u5C11\u5B9E\u65F6\u65F6\u949F\uFF08RTC\uFF09\u3002\
  \u7136\u800C\uFF0C\u53EF\u4EE5\u4F7F\u7528\u5916\u90E8RTC\u6A21\u5757\uFF08\u5982\
  DS3231\uFF09\u548C\u5E93\uFF08\u4F8B\u5982Adafruit\u5F00\u53D1\u7684`RTClib`\uFF09\
  \uFF0C\u8FD9\u4F7F\u5F97\u4E0E\u8FD9\u4E9B\u6A21\u5757\u7684\u63A5\u53E3\u53D8\u5F97\
  \u7B80\u5355\u3002 \u9996\u5148\uFF0C\u786E\u4FDD`RTClib`\u5E93\u5B89\u88C5\u5728\
  \u4F60\u7684Arduino\u2026"
lastmod: '2024-03-13T22:44:48.074256-06:00'
model: gpt-4-0125-preview
summary: "Arduino\u672C\u8EAB\u6CA1\u6709\u5185\u7F6E\u7684\u65B9\u6CD5\u53EF\u4EE5\
  \u76F4\u63A5\u83B7\u53D6\u5F53\u524D\u65E5\u671F\uFF0C\u56E0\u4E3A\u5B83\u7F3A\u5C11\
  \u5B9E\u65F6\u65F6\u949F\uFF08RTC\uFF09\u3002\u7136\u800C\uFF0C\u53EF\u4EE5\u4F7F\
  \u7528\u5916\u90E8RTC\u6A21\u5757\uFF08\u5982DS3231\uFF09\u548C\u5E93\uFF08\u4F8B\
  \u5982Adafruit\u5F00\u53D1\u7684`RTClib`\uFF09\uFF0C\u8FD9\u4F7F\u5F97\u4E0E\u8FD9\
  \u4E9B\u6A21\u5757\u7684\u63A5\u53E3\u53D8\u5F97\u7B80\u5355."
title: "\u83B7\u53D6\u5F53\u524D\u65E5\u671F"
weight: 29
---

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
