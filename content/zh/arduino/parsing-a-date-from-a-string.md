---
title:                "从字符串解析日期"
date:                  2024-01-20T15:34:30.586419-07:00
html_title:           "Arduino: 从字符串解析日期"
simple_title:         "从字符串解析日期"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (是什么 & 为什么?)
解析日期就是从字符串中提取日期信息。程序员这么做是为了能够处理和存储人们可读的日期格式。

## How to (操作方法):
下面是一个Arduino程序，它展示了如何将字符串形式的日期解析为单独的天、月、年。

```Arduino
#include <Wire.h>
#include <RTClib.h>

// 假定日期格式为 "DD/MM/YYYY"
String dateString = "23/04/2023";

void setup() {
  Serial.begin(9600);

  int day = dateString.substring(0, 2).toInt();
  int month = dateString.substring(3, 5).toInt();
  int year = dateString.substring(6, 10).toInt();

  Serial.print("Day: ");
  Serial.println(day);
  Serial.print("Month: ");
  Serial.println(month);
  Serial.print("Year: ");
  Serial.println(year);
}

void loop() {
  // 这里没有代码，因为仅在setup中解析日期
}
```

可能的输出：

```
Day: 23
Month: 4
Year: 2023
```

## Deep Dive (深入探索):
字符串中的日期解析最早出现于计算机起始阶段，目的是转化为计算机能处理的格式。除了上述方法，Arduino 也可以使用第三方库处理更复杂的日期字符串，比如 `TimeLib.h`。实现上，解析涉及了字符串操作和类型转换。

## See Also (另请参阅):
- Arduino 官方文档关于字符串处理的教程: https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/
- `TimeLib.h` 库的 GitHub 仓库: https://github.com/PaulStoffregen/Time
- 更多关于 RTClib 的信息和用法：https://github.com/adafruit/RTClib
