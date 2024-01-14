---
title:                "Arduino: 获取当前日期"
programming_language: "Arduino"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 为什么

在现代的生活中，我们经常需要使用到当前的日期。无论是日历、日程安排还是其他的应用，都需要准确地获取当前的日期。在编写Arduino程序时，也经常需要获取当前的日期来做一些特定的操作。因此，学习如何获取当前日期是非常有用的。

## 如何获取当前日期

获取当前日期的方式在Arduino程序中并不复杂，只需要使用`millis()`函数即可。这个函数可以返回自Arduino开发板上电以来的毫秒数。根据这个毫秒数，我们可以计算出当前日期，并且将它打印出来。下面是一个简单的例子：

```Arduino
unsigned long currentMillis = millis();
unsigned long days = currentMillis / (24 * 60 * 60 * 1000);
unsigned long hours = (currentMillis % (24 * 60 * 60 * 1000)) / (60 * 60 * 1000);
unsigned long minutes = (currentMillis % (60 * 60 * 1000)) / (60 * 1000);
unsigned long seconds = (currentMillis % (60 * 1000)) / 1000;

Serial.print("Current date: ");
Serial.print(days);
Serial.print(" days, ");
Serial.print(hours);
Serial.print(" hours, ");
Serial.print(minutes);
Serial.print(" minutes, and ");
Serial.print(seconds);
Serial.println(" seconds since power on.");
```

输出结果可能会是这样的：

```
Current date: 8 days, 3 hours, 21 minutes, and 42 seconds since power on.
```

## 深入了解获取当前日期

除了使用`millis()`函数外，还有其他一些方法可以获取当前日期。例如，可以使用`clock()`函数来获取系统时钟的值，然后根据系统时钟的值来计算当前日期。另外，也可以借助于DS1307实时时钟模块来获取真正的日期和时间。

此外，我们还可以使用`day()`,`month()`,`year()`等函数来分别获取当前日期的日、月、年部分。这些函数需要传入一个时间戳作为参数，可以通过获取当前毫秒数然后转换成时间戳的方式来使用。

## 参考资料

- [Arduino官方文档中的millis()函数](https://www.arduino.cc/reference/en/language/functions/time/millis/)
- [Arduino官方文档中的clock()函数](https://www.arduino.cc/reference/en/language/functions/time/clock/)
- [Arduino官方文档中的day()函数](https://www.arduino.cc/reference/en/language/functions/time/day/)
- [DS1307实时时钟模块使用指南](https://www.arduino.cc/en/tutorial/DS1307RealTimeClock)
- [如何使用Arduino实现一个日历应用](https://www.instructables.com/id/Programming-the-7Science-RTC-Module-Using-Arduino/)