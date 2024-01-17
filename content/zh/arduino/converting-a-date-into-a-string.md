---
title:                "将日期转换为字符串"
html_title:           "Arduino: 将日期转换为字符串"
simple_title:         "将日期转换为字符串"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 什么是日期转换为字符串？为什么要这样做？
日期转换为字符串是将日期数据转换为字符串格式的过程。程序员这样做是为了能够更方便地处理日期数据，以便于在程序中使用和显示。

## 如何进行日期转换为字符串：
```Arduino
// 设置日期对象
DateTime now = RTC.now();
// 使用String对象创建字符串，格式为年-月-日
String date_str = String(now.year(), DEC) + "-" + String(now.month(), DEC) + "-" + String(now.day(), DEC);
// 打印输出日期字符串
Serial.print(date_str);
// 输出结果为：2021-12-31
```

## 深入了解：
日期转换为字符串的历史背景可以追溯到机器语言编程时代，当时的计算机只能处理数字，所以日期也只能以数字的形式存储和显示。随着程序语言的发展，日期转换为字符串的方式也逐渐多样化，如使用各种格式字符串库、内置函数等。在Arduino中，可以使用String对象来将日期转换为字符串，也可以使用其他库如Time库来实现。

## 参考资料：
- [Arduino官方文档-日期转换为字符串](https://www.arduino.cc/en/Tutorial/StringConversions) 
- [Arduino官方文档-日期函数](https://www.arduino.cc/en/Reference/DateTime)
- [时间库 Time库](https://playground.arduino.cc/code/time/)
- [字符串库 String库](https://playground.arduino.cc/Main/StringObject/)