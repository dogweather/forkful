---
title:                "Arduino: 将日期转换为字符串"
simple_title:         "将日期转换为字符串"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

##为什么

在Arduino编程中，有时我们会需要将日期转换为字符串（string）的形式。这可能是为了打印日期，记录日志，或者与其他数据进行比较。通过将日期转换为字符串，我们可以方便地在程序中使用和处理日期数据。

##如何做

```Arduino
#include <DateTime.h> //引入DateTime库
DateTime now = DateTime(now()); //获取当前日期
int year = now.year(); //提取年份
int month = now.month(); //提取月份
int day = now.day(); //提取日期
String date = String(year) + "/" + String(month) + "/" + String(day); //将日期转换为字符串形式
//输出结果为 "YYYY/MM/DD"
```

以上是一个简单的示例，通过使用DateTime库和一些基本的字符串操作，我们可以将日期数据转换为指定格式的字符串。需要注意的是，DateTime库需要提前下载并在程序中引入。

##深入探讨

日期数据虽然看似简单，但在编程中却需要特别注意。例如，在转换为字符串之前，我们需要确保日期数据的格式和大小写是统一的。否则，可能会导致转换出错或者无法与其他数据进行比较。

此外，如果我们需要打印日期，我们还需要考虑不同国家地区的时间格式差异，以及使用不同的DateTime库来适配不同的时区。

总的来说，日期转换为字符串并不复杂，但是需要我们仔细处理和考虑各种情况，以确保程序的稳定性。

##请参考

- [DateTime库下载](https://github.com/xoseperez/DateTime)
- [DateTime库文档](https://github.com/xoseperez/DateTime/wiki)
- [DateTime库简介](https://diyprojects.io/diy-arduino-calendar-date-hours-minutes-seconds-ds3231-display/#.XvZgTpMzY0M)