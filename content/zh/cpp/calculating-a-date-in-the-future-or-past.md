---
title:                "计算未来或过去的日期."
html_title:           "C++: 计算未来或过去的日期."
simple_title:         "计算未来或过去的日期."
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 为什么

在日常生活中，我们经常需要计算未来或过去的日期，例如计算下个月的生日是星期几，或者确认某个事件是几天前发生。编程可以帮助我们更快速和准确地进行这些计算，从而帮助我们更好地安排日常生活。

## 如何

计算日期涉及到大量的数学计算，但是使用C++编程语言可以让这个过程更简单和高效。首先，我们需要定义一个结构体来存储日期的信息，包括年、月和日。然后，我们可以使用if语句和循环来判断输入的日期是否合法，并计算出未来或过去的日期。

```C++
// 定义结构体存储日期信息
struct Date {
    int year;
    int month;
    int day;
};

// 判断是否是闰年
bool isLeapYear(int year) {
    if ((year % 4 == 0 && year % 100 != 0) || year % 400 == 0) {
        return true;
    } else {
        return false;
    }
}

// 计算未来一天的日期
void calculateFutureDate(Date currentDate) {
    // 判断是否是2月
    if (currentDate.month == 2) {
        // 判断是否是闰年
        if (isLeapYear(currentDate.year)) {
            // 如果是闰年且日期为29号，则转跳到3月
            if (currentDate.day == 29) {
                currentDate.month++;
                currentDate.day = 1;
            } else {
                // 否则日期加一
                currentDate.day++;
            }
        } else {
            // 不是闰年且日期为28号，则转跳到3月
            if (currentDate.day == 28) {
                currentDate.month++;
                currentDate.day = 1;
            } else {
                // 否则日期加一
                currentDate.day++;
            }
        }
    } else {
        // 判断是否是31天的大月份
        if ((currentDate.month == 1 || currentDate.month == 3 || currentDate.month == 5 || currentDate.month == 7 || currentDate.month == 8 || currentDate.month == 10 || currentDate.month == 12) && currentDate.day == 31) {
            // 转跳到下一个月
            if (currentDate.month == 12) {
                currentDate.year++;
                currentDate.month = 1;
            } else {
                currentDate.month++;
            }
            currentDate.day = 1;
        } else if (currentDate.day == 30) {
            // 转跳到下一个月（4、6、9、11月）
            currentDate.month++;
            currentDate.day = 1;
        } else {
            // 日期加一
            currentDate.day++;
        }
    }
}

// 计算过去一天的日期
void calculatePastDate(Date currentDate) {
    if (currentDate.day == 1) {
        if (currentDate.month == 1) {
            // 转跳到上一年
            currentDate.year--;
            currentDate.month = 12;
            currentDate.day = 31;
        } else {
            // 转跳到上一个月
            currentDate.month--;
            // 判断是否是31天的大月份
            if (currentDate.month == 1 || currentDate.month == 3 || currentDate.month == 5 || currentDate.month == 7 || currentDate.month == 8 || currentDate.month == 10 || currentDate.month == 12) {
                currentDate.day = 31;
            } else if (currentDate.month == 2) {
                // 判断是否是闰年
                if (isLeapYear(currentDate.year)) {
                    currentDate.day = 29;
                } else {
                    currentDate.day = 28;
                }
            } else {
                // 30天的小月份
                currentDate.day = 30;
            }
        }
    } else {
        // 日期减一
        currentDate.day--;
    }
}

// 示例输入日期为1月1日
Date currentDate = {2019, 1, 1};

// 计算未来一天的日期
calculateFutureDate(currentDate);
cout << "未来一天的日期为：" << currentDate.year << "-" << currentDate.month << "-" << currentDate.day << endl;

// 计算