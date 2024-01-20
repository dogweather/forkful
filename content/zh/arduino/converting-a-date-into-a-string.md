---
title:                "将日期转换为字符串"
html_title:           "Bash: 将日期转换为字符串"
simple_title:         "将日期转换为字符串"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？ （What & Why?）

日期转字符串是将日期数据转换为可读文本形式的过程。程序员这么做是因为，这可以让人更容易理解和操作日期数据。

## 如何： （How to:）

```Arduino
#include <TimeLib.h>
#include <Time.h>

void setup() {
  Serial.begin(9600);
  
  setTime(8, 29, 0, 15, 10, 2021); //设置时间和日期为：2021年10月15日上午8:29。
                                   
}

void loop() {
  time_t t = now();
  String dateStr = String(day(t)) + "-" +
                   String(month(t)) + "-" +
                   String(year(t)); //将日期转换为“日-月-年”格式的字符串。
 
  Serial.println(dateStr);
  delay(1000);
}
```

串口监视器的输出将会是：“15-10-2021”。

## 深入探索 （Deep Dive）

历史背景上，日期转字符串的操作常常被用在不同的计算机程序中，其核心目的是为了人机交互的便捷性。至于替代方案，如果数据的表示形式没有严格要求，那么UNIX时间戳也是一种选择。它是从1970年1月1日开始按秒计算的时间，很适合于计算机程序间的交互。至于实现细节，你可以使用Arduino的Time库，它已包含了大部分你所需要的日期和时间操作功能。

## 另见 （See Also）

1. Time库的GitHub仓库：[https://github.com/PaulStoffregen/Time](https://github.com/PaulStoffregen/Time)
3. 关于Unix时间戳的更多信息： [https://www.unixtimestamp.com/](https://www.unixtimestamp.com/)