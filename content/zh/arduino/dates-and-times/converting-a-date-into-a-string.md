---
date: 2024-01-20 17:35:45.402097-07:00
description: "How to: (\u5982\u4F55\u64CD\u4F5C\uFF1A) \u4F7F\u7528Arduino\u4E2D\u7684\
  \ `sprintf` \u51FD\u6570\u6216\u8005 `String` \u7C7B\u53EF\u4EE5\u8F7B\u677E\u5B8C\
  \u6210\u8F6C\u6362\u3002\u8BF7\u770B\u4EE5\u4E0B\u4F8B\u5B50\uFF1A."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:48.075332-06:00'
model: gpt-4-1106-preview
summary: "\u4F7F\u7528Arduino\u4E2D\u7684 `sprintf` \u51FD\u6570\u6216\u8005 `String`\
  \ \u7C7B\u53EF\u4EE5\u8F7B\u677E\u5B8C\u6210\u8F6C\u6362\u3002\u8BF7\u770B\u4EE5\
  \u4E0B\u4F8B\u5B50\uFF1A."
title: "\u5C06\u65E5\u671F\u8F6C\u6362\u4E3A\u5B57\u7B26\u4E32"
weight: 28
---

## How to: (如何操作：)
使用Arduino中的 `sprintf` 函数或者 `String` 类可以轻松完成转换。请看以下例子：

```Arduino
char formattedDate[20];
int year = 2023, month = 4, day = 2;

sprintf(formattedDate, "%04d-%02d-%02d", year, month, day);
Serial.println(formattedDate);
// 输出: 2023-04-02

String dateString = String(year) + "-" + 
                    (month < 10 ? "0" + String(month) : String(month)) + "-" + 
                    (day < 10 ? "0" + String(day) : String(day));
Serial.println(dateString);
// 输出: 2023-04-02
```

## Deep Dive (深度解析)
过去，Arduino里没有现成的日期转字符串功能，所以开发者必须手动格式化字符串。现在，`sprintf` 或者 `String` 类的操作让这事儿变简单，但并不是所有Arduino板子的 `sprintf` 都支持浮点数。备选方案，如使用第三方时间库（比如 `TimeLib.h` 或 `RTClib.h`）也能得到转换后的字符串，这样可以提供更多格式化的选项。

实现时，注意内存限制。字符串操作可能会占用较多的RAM，这对内存较小的设备来说是个挑战。

## See Also (另请参阅)
- Arduino的字符串文档：[https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/](https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/)
- TimeLib库：[https://www.pjrc.com/teensy/td_libs_Time.html](https://www.pjrc.com/teensy/td_libs_Time.html)
- RTClib库：[https://github.com/adafruit/RTClib](https://github.com/adafruit/RTClib)
