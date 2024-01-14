---
title:                "Arduino: 计算过去或未来的日期"
programming_language: "Arduino"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# 为什么
很多人可能会问，为什么要在Arduino编程中计算未来或过去的日期？其实，这是一个很有用的技巧，能够帮助你更精确地控制程序的运行时间，也方便你在设计各种计时功能时使用。

# 怎样操作
在Arduino中，我们可以使用内置函数"```millis()```"来获取当前的系统时间，单位是毫秒。我们可以通过一些计算来将这一数值转换成日期格式，从而实现时间的计算。下面是一个简单的范例代码：

```
Arduino的库文件中有一个名为 "RTClib" 的库，可以直接在IDE中安装。在这个库的帮助下，我们可以轻松地获取当前的日期和时间。比如，想要在5秒后打印出当日的日期，可以这样写：

```
#include <RTClib.h>
RTC_DS3231 rtc;

void setup() {
  Serial.begin(9600);

  // 初始化RTC模块
  rtc.begin();

  // 获取当前时间
  DateTime now = rtc.now();

  // 计算5秒后的日期
  DateTime future = now + TimeSpan(0,0,5,0);

  // 打印出未来日期的格式
  Serial.println(future.format("m.d.Y"));
}

void loop() {
  // 空函数
}
```

在上面的代码中，我们使用了"```RTClib```"库来初始化了RTC模块，并且通过"```DateTime```"类来获取系统当前的日期和时间。然后，我们使用"```TimeSpan```"类来计算出距离当前日期5秒后的时间，并将其赋值给"```future```"变量。最后，我们使用"```format()```"函数来设置日期的格式，并使用"```Serial.println()```"函数打印出结果。运行代码会在串口监视器中输出类似"```12.17.2021```"的结果。

# 深入学习
当然，我们可以通过使用不同的数值来计算出未来或过去的日期，而不仅仅是5秒。同时，我们也可以通过一些逻辑判断，来确定未来或过去的日期是否为特定的日期，从而实现一些更复杂的计时功能。

# 参考链接
- Arduino的官方文档：https://www.arduino.cc/en/Reference/DueDate
- "```millis()```"函数的使用方法：https://www.arduino.cc/reference/en/language/functions/time/millis/
- "```RTClib```"库的GitHub页面：https://github.com/adafruit/RTClib