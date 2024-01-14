---
title:                "Arduino: 将日期转换为字符串"
programming_language: "Arduino"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

为什么：

很多时候，我们在Arduino程序中需要将日期转换成字符串，比如为了在显示屏上显示日期，或者将日期作为文件名保存数据。因此，了解如何在Arduino中完成这个过程是非常有用的。

## 如何：

```Arduino
// 第一个示例
int day = 10;
int month = 9;
int year = 2021;

String date = String(day) + "/" + String(month) + "/" + String(year); // 将日期转换成字符串
Serial.println(date); // 将日期打印出来

// 输出：10/9/2021

// 第二个示例
int hour = 12;
int minute = 30;
int second = 45;

String time = String(hour) + ":" + String(minute) + ":" + String(second); // 将时间转换成字符串
Serial.println(time); // 将时间打印出来

// 输出：12:30:45
```

在上面的示例中，我们先将日期和时间的各个部分（日、月、年、时、分、秒）转换成字符串，然后再使用字符串拼接的方式将它们组合在一起。这样就可以将日期和时间转换成字符串，方便在Arduino中进行操作和显示。

## 深入了解：

除了上面的示例中使用的方法，还有其他的方式可以将日期和时间转换成字符串。比如通过使用DateTime库中的函数，或者自定义函数来实现。另外，还可以对字符串进行格式化，比如设置日期的显示格式为yyyy-mm-dd，时间的显示格式为hh:mm:ss等。了解这些更多的方法可以帮助我们更灵活地在Arduino中处理日期和时间的字符串转换。

## 参考链接：

- [Arduino官方教程-String类型](https://www.arduino.cc/reference/en/language/variables/data-types/string/)
- [使用DateTime库处理日期和时间](https://www.robotshop.com/community/forum/t/display-rtc-ds1307-date-and-time-arduino)
- [自定义函数实现日期和时间的字符串转换](https://www.electronicwings.com/arduino/basics-string-functions-in-arduino-ide)

## 参见：

- [Arduino官方教程-String类型](https://www.arduino.cc/reference/en/language/variables/data-types/string/)
- [DateTime库文档](https://github.com/PaulStoffregen/DateTime)
- [Arduino字符串函数指南](https://www.electronicwings.com/arduino/basics-string-functions-in-arduino-ide)