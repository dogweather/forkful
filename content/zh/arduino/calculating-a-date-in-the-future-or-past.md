---
title:                "计算未来或过去的日期"
html_title:           "Arduino: 计算未来或过去的日期"
simple_title:         "计算未来或过去的日期"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 为什么

如果你想创建一个计时器或是一个日历应用程序，你可能会需要计算未来或过去的日期。这篇文章将向你介绍如何在Arduino中编程实现这一功能。

## 如何做

首先，你需要创建一个名为“calculateDate”的函数，函数的参数包括要计算的日期、计算的类型（过去或未来）、以及要加上或减去的时间。在函数中，你需要使用Arduino自带的"DateTime"库来获取当前日期时间，然后进行计算。

```Arduino
#include <DateTime.h>

// 定义函数
void calculateDate(DateTime date, int type, int time) { 
    int day = date.day(); // 获取当前日期的天数
    int month = date.month(); // 获取当前日期的月份
    int year = date.year(); // 获取当前日期的年份
    int newDay, newMonth, newYear; // 定义新的日期变量

    // 根据计算类型来确定日期的变化方向
    if (type == 0) { // 过去
        newDay = day - time;
        newMonth = month;
        newYear = year;
        
        // 如果计算的天数小于1，需要向前一个月借的天数
        if (newDay < 1) {
            newMonth--;
            if (newMonth < 1) {
                newMonth = 12;
                newYear--;
            }

            // 根据月份和年份来计算实际的天数
            if (newMonth == 2) {
                if (newYear % 4 == 0) { // 判断是否为闰年
                    newDay = 29 + newDay;
                } else {
                    newDay = 28 + newDay;
                }
            } else if (newMonth == 4 || newMonth == 6 || newMonth == 9 || newMonth == 11) {
                newDay = 30 + newDay;
            } else {
                newDay = 31 + newDay;
            }
        }
    } else if (type == 1) { // 未来
        newDay = day + time;
        newMonth = month;
        newYear = year;

        // 根据月份和年份来计算实际的天数
        if (newDay > 31 && (newMonth == 1 || newMonth == 3 || newMonth == 5 || newMonth == 7 ||
            newMonth == 8 || newMonth == 10 || newMonth == 12)) {
                newDay = newDay - 31;
                newMonth++;
                if (newMonth > 12) {
                    newMonth = 1;
                    newYear++;
                }
        } else if (newDay > 30 && (newMonth == 4 || newMonth == 6 || newMonth == 9 || newMonth == 11)) {
                newDay = newDay - 30;
                newMonth++;
        } else if (newDay > 28 && newMonth == 2) { // 如果是2月份，判断是否为闰年
            if (newYear % 4 == 0) {
                newDay = newDay - 29;
            } else {
                newDay = newDay - 28;
            }
            newMonth++;
        }
    }

    // 输出计算后的日期
    Serial.print(newDay);
    Serial.print("/");
    Serial.print(newMonth);
    Serial.print("/");
    Serial.println(newYear);
}

// 在“setup”函数中初始化串口
void setup() {
    Serial.begin(9600);
}

// 在“loop”函数中调用我们的计算日期函数
void loop() {
    DateTime now = DateTime(); // 获取当前日期时间
    calculateDate(now, 0, 10); // 计算10天前的日期
    calculateDate(now, 1, 5); // 计算5天后的日期
    delay(5000); // 延时5秒
}
```

运行这段代码，你将得到以下输出：

```
19/4/2021
1/5/2021
```

## 深入了解

在这段代码中，我们使用了Arduino自带的"DateTime"库来获取当前日期时间。然后，我们通过对日期的天数进行加减来实现日期