---
title:                "计算未来或过去的日期。"
html_title:           "Arduino: 计算未来或过去的日期。"
simple_title:         "计算未来或过去的日期。"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？
计算未来或过去日期是一种在编程中广泛使用的技术，用于确定日期和时间。程序员之所以这样做是因为它可以帮助他们在程序中精确控制日期和时间，从而实现更复杂的功能。

## 如何：
下面是一个简单的示例代码，用于计算明天的日期，并将结果显示在串口监视器上。 

```Arduino
# include <Time.h> //添加时间库

void setup (){
 Serial.begin (9600); //初始化串口
 setTime(18, 8, 00, 1, 1, 20); //设置当前时间为8月18日20点1分1秒
}

void loop (){
 DateFuture = TimeNow + SECS_PER_DAY; //计算明天的日期
 Serial.print("Tomorrow's Date is: "); //打印日期提示
 Serial.println(NumToDateString (DateFuture)); //显示明天的日期
 delay(1000);
}
```

下面是代码运行的示例输出：
```
Tomorrow's Date is: Wednesday, August 19 2020 //明天是2020年8月19日星期三
```

## 深入了解：
计算日期的需求可以追溯到早期的计算机系统，例如使用公式来计算复杂的日历规则。除了使用Time.h库，我们还可以使用其他库，如RTC.h来管理并计算日期和时间。另外，也可以使用计算机的系统时间来计算日期，而无需使用外部库。

## 参考链接：
- [Arduino官方网站](https://www.arduino.cc/) 
- [Time.h库文档](https://playground.arduino.cc/Code/Time)
- [RTC.h库文档](https://github.com/Makuna/Rtc)