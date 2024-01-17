---
title:                "获取当前日期"
html_title:           "Arduino: 获取当前日期"
simple_title:         "获取当前日期"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 缘由 & 原因：
获取当前日期是指在编程中获取当前的日期信息，程序员通常这么做是为了在程序中使用当前日期来做某些操作。

## 怎么做：
在Arduino语言中，使用内置的函数来获取当前日期。下面是一个简单的例子，展示如何在控制台打印出当前日期：
```Arduino
#include <Time.h>  // 导入Time库
void setup() {
  Serial.begin(9600);  // 设置串口波特率
  setTime(12, 34, 56, 7, 8, 2021);  // 设置日期和时间
}
void loop() {
  Serial.print("The current date is: ");
  Serial.print(day());
  Serial.print("/");
  Serial.print(month());
  Serial.print("/");
  Serial.println(year());  // 分别打印出当前日期的日、月、年
  delay(1000);  // 延迟1秒后重复打印
}
```
运行上面的代码，控制台将输出：The current date is: 8/7/2021，表示当前日期是2021年8月7日。

## 深入了解：
获取当前日期的方法有很多种，除了使用内置函数，还可以通过连接网络来获取网络时间。Arduino也支持使用RTC（Real Time Clock）模块来获取当前日期和时间。另外，历法的实现也会影响到获取当前日期的结果。

## 参考资料：
- Arduino文档：https://www.arduino.cc/reference/en/language/functions/time/
- 极客时间：http://www.geektime.com.cn/post/110152
- RTC模块：https://makerpro.cc/2017/08/tutorial-rtc-module-arduino/