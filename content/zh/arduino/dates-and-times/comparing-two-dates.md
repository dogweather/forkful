---
date: 2024-01-20 17:32:21.998867-07:00
description: "How to: \u600E\u4E48\u505A\uFF1F \u6BD4\u8F83\u65E5\u671F\u7684\u9700\
  \u6C42\u4FC3\u6210\u4E86\u7535\u8111\u5386\u53F2\u4E2D\u65F6\u95F4\u7BA1\u7406\u529F\
  \u80FD\u7684\u53D1\u5C55\u3002\u5728\u6CA1\u6709\u5185\u5EFA\u65F6\u95F4\u529F\u80FD\
  \u7684\u7535\u8111\u786C\u4EF6\u4E0A\uFF0C\u5982\u65E9\u671F\u7684Arduino\u677F\uFF0C\
  \u5B9E\u65F6\u65F6\u949F\uFF08RTC\uFF09\u6A21\u5757\u88AB\u7528\u6765\u8DDF\u8E2A\
  \u5B9E\u9645\u65F6\u95F4\u3002\u4F7F\u7528\u5E93\u5982TimeLib\uFF0C\u80FD\u7B80\u5316\
  \u65E5\u671F\u6BD4\u8F83\u7684\u6D41\u7A0B\u3002\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T22:38:47.232085-06:00'
model: gpt-4-1106-preview
summary: "\u600E\u4E48\u505A\uFF1F \u6BD4\u8F83\u65E5\u671F\u7684\u9700\u6C42\u4FC3\
  \u6210\u4E86\u7535\u8111\u5386\u53F2\u4E2D\u65F6\u95F4\u7BA1\u7406\u529F\u80FD\u7684\
  \u53D1\u5C55\u3002\u5728\u6CA1\u6709\u5185\u5EFA\u65F6\u95F4\u529F\u80FD\u7684\u7535\
  \u8111\u786C\u4EF6\u4E0A\uFF0C\u5982\u65E9\u671F\u7684Arduino\u677F\uFF0C\u5B9E\u65F6\
  \u65F6\u949F\uFF08RTC\uFF09\u6A21\u5757\u88AB\u7528\u6765\u8DDF\u8E2A\u5B9E\u9645\
  \u65F6\u95F4\u3002\u4F7F\u7528\u5E93\u5982TimeLib\uFF0C\u80FD\u7B80\u5316\u65E5\u671F\
  \u6BD4\u8F83\u7684\u6D41\u7A0B\u3002"
title: "\u6BD4\u8F83\u4E24\u4E2A\u65E5\u671F"
weight: 27
---

## How to: 怎么做？
```Arduino
#include <TimeLib.h>

void setup() {
  Serial.begin(9600);
  setTime(8, 29, 0, 5, 11, 2023); // 设置当前时间为 2023年11月5日 8:29:00
  time_t t1 = now(); // 获取当前时间

  setTime(9, 15, 0, 12, 1, 2024); // 设置另一个时间为 2024年1月12日 9:15:00
  time_t t2 = now(); // 获取设置的时间

  compareDates(t1, t2);
}

void loop() {
  // 主程序循环为空，因为我们不需要连续比较日期
}

void compareDates(time_t d1, time_t d2) {
  if(d1 > d2) {
    Serial.println("Date1 is after Date2");
  } else if(d1 < d2) {
    Serial.println("Date1 is before Date2");
  } else {
    Serial.println("Dates are equal");
  }
}
```
示例输出：
```
Date1 is before Date2
```

## Deep Dive: 深入了解
比较日期的需求促成了电脑历史中时间管理功能的发展。在没有内建时间功能的电脑硬件上，如早期的Arduino板，实时时钟（RTC）模块被用来跟踪实际时间。使用库如TimeLib，能简化日期比较的流程。

可选方法包括使用简单的整数比较，如上面的例子，或利用更复杂的时间库来处理闰年和时区等问题。在Arduino中，程序员通常会将时间以Unix时间戳格式（从1970年1月1日起算的秒数）来存储和比较，因为它简化了比较过程。

## See Also: 参考链接
- Arduino Time Library: https://www.arduino.cc/reference/en/libraries/time/
- DS3231 RTC Arduino Module (常用的RTC模块): https://www.arduino.cc/en/Guide/Libraries#toc4
- Unix Time and its importance in Programming: http://unixtimestamp.com/
