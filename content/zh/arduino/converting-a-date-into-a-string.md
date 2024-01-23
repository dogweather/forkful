---
title:                "将日期转换为字符串"
date:                  2024-01-20T17:35:45.402097-07:00
model:                 gpt-4-1106-preview
simple_title:         "将日期转换为字符串"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

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
