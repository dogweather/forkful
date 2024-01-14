---
title:    "Arduino: 获取当前日期"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/getting-the-current-date.md"
---

{{< edit_this_page >}}

##为什么:
在现在的世界，时间意味着一切。要在Arduino编程中使用正确的时间，可以让你的项目更准确和可靠。不管是跟踪时间敏感的事件，或者简单地显示当前日期，获取当前日期是非常必要的。

##如何做:
在Arduino编程中，有一个很简单的方式来获取当前日期，即使用内置的函数 `millis()`。这个函数返回自Arduino板启动时间以来的毫秒数，它可以让我们很容易地计算出当前的日期。下面是一个使用 `millis()` 函数获取当前日期并显示在串口监视器的示例代码:

```
Arduino
//获取当前日期并显示在串口监视器
unsigned long currentTime = millis(); //获取自开机以来的毫秒数
unsigned long seconds = (currentTime / 1000) % 60; //计算当前秒数
unsigned long minutes = (currentTime / (1000 * 60)) % 60; //计算当前分钟数
unsigned long hours = (currentTime / (1000 * 60 * 60)) % 24; //计算当前小时数
unsigned long days = (currentTime / (1000 * 60 * 60 * 24)) % 365; //计算当前日期
Serial.print("Current date: ");
Serial.print(days);
Serial.print(" days, ");
Serial.print(hours);
Serial.print(" hours, ");
Serial.print(minutes);
Serial.print(" minutes, ");
Serial.print(seconds);
Serial.println(" seconds"); 
```

运行示例代码后，串口监视器将显示类似于以下内容:

```
Current date: 0 days, 0 hours, 0 minutes, 10 seconds
```

这表示当前为启动后的10秒。

##深入探讨:
使用 `millis()` 函数虽然很简单，但它有一个缺点，那就是它只能返回毫秒数，而无法直接得到日期、小时和分钟等信息。因此，使用 `millis()` 函数获取当前日期还需要进行一些运算。如果你想要直接获取当前的日期，可以考虑使用第三方的Real Time Clock (RTC)模块，它具有精确的日期和时间功能，可以与Arduino板连接并轻松获取当前日期。如果你想进一步了解如何使用RTC模块，可以查看以下链接:

- [RTC模块教程](https://maker.pro/arduino/projects/how-to-use-a-real-time-clock-module-with-arduino)

##另请参阅:
- [Arduino文档：millis()函数](https://www.arduino.cc/reference/en/language/functions/time/millis/)
- [Arduino教程：串口监视器(Serial Monitor)](https://www.arduino.cc/en/Tutorial/SerialMonitor)
- [Arduino论坛](https://forum.arduino.cc/)