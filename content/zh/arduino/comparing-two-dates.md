---
title:                "Arduino: 比较两个日期"
simple_title:         "比较两个日期"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 为什么

在使用Arduino编程时，我们经常需要比较两个日期的大小。比如说在控制LED灯的闪烁频率或者控制电机转动方向时，我们就需要比较日期来决定程序的执行逻辑。因此，掌握如何比较日期是很重要的一项技能。

## 如何

下面我将介绍如何使用Arduino比较两个日期的大小，并给出一个简单的示例来演示。

首先，我们定义两个日期变量，分别为date1和date2，假设它们分别代表了2019年10月1日和2019年10月2日。然后，我们可以利用Arduino的“大于”和“小于”运算符（>和<）来比较这两个日期的大小。代码如下所示：

```Arduino
int date1 = 20191001;
int date2 = 20191002;

if (date1 < date2) {
  Serial.println("Date1 is earlier than date2!"); //通过串口打印信息
} else if (date1 > date2) {
  Serial.println("Date2 is earlier than date1!");
} else {
  Serial.println("Date1 and date2 are the same.");
}
```

执行以上代码，我们可以看到串口输出了“Date1 is earlier than date2!”，说明我们成功比较了两个日期的大小。

## 深入探讨

在比较日期时，我们通常会使用整型变量存储日期，如示例中的20191001和20191002。这是因为日期可以简单地表示为数字，如“20191001”代表2019年10月1日。此外，我们也可以使用其他的数据类型来存储日期，比如字符串或者结构体。

在Arduino中，要正确比较两个日期的大小，我们还需要注意日期的表示形式。比如20191001和191001表示的是同一天，但用整型比较时会得到不同的结果。在实际应用中，我们最好统一使用同一种日期表示形式来避免混淆。

## 参考资料

如果你对Arduino中日期比较的更多细节感兴趣，可以参考以下链接：

- [Arduino日期函数](https://www.arduino.cc/reference/en/language/functions/time/date/)
- [日期比较的实例分析](https://howtomechatronics.com/tutorials/arduino/arduino-date-and-time-tutorial/)
- [日期和时间标准库](http://www.cplusplus.com/reference/ctime/)

## 参见

- [Arduino官方网站](https://www.arduino.cc/)
- [Arduino编程入门教程](https://blog.adminite.com/arduino-programming-tutorial-beginners/)
- [Arduino论坛](https://forum.arduino.cc/)