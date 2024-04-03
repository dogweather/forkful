---
date: 2024-01-20 17:35:45.402097-07:00
description: "\u5C06\u65E5\u671F\u8F6C\u6362\u4E3A\u5B57\u7B26\u4E32\uFF0C\u5C31\u662F\
  \u628A\u65E5\u671F\u683C\u5F0F\u4E13\u6210\u4E00\u4E32\u7279\u5B9A\u683C\u5F0F\u7684\
  \u6587\u672C\u3002\u7A0B\u5E8F\u5458\u8FD9\u4E48\u505A\u662F\u4E3A\u4E86\u6613\u4E8E\
  \u663E\u793A\u3001\u5B58\u50A8\u6216\u8005\u5728\u7F51\u7EDC\u4E0A\u4F20\u8F93\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:48.075332-06:00'
model: gpt-4-1106-preview
summary: "\u5C06\u65E5\u671F\u8F6C\u6362\u4E3A\u5B57\u7B26\u4E32\uFF0C\u5C31\u662F\
  \u628A\u65E5\u671F\u683C\u5F0F\u4E13\u6210\u4E00\u4E32\u7279\u5B9A\u683C\u5F0F\u7684\
  \u6587\u672C\u3002\u7A0B\u5E8F\u5458\u8FD9\u4E48\u505A\u662F\u4E3A\u4E86\u6613\u4E8E\
  \u663E\u793A\u3001\u5B58\u50A8\u6216\u8005\u5728\u7F51\u7EDC\u4E0A\u4F20\u8F93\u3002\
  ."
title: "\u5C06\u65E5\u671F\u8F6C\u6362\u4E3A\u5B57\u7B26\u4E32"
weight: 28
---

## What & Why? (是什么？为什么？)

将日期转换为字符串，就是把日期格式专成一串特定格式的文本。程序员这么做是为了易于显示、存储或者在网络上传输。

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
