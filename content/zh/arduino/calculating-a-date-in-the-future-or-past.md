---
title:    "Arduino: 计算未来或过去的日期"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# 为什么要使用Arduino计算日期？
使用Arduino计算日期可以帮助您在您的项目中确定特定日期，例如确定未来或过去的日期。这对于制作日历或倒计时器非常有用。

## 如何使用Arduino计算日期？
您可以使用以下代码来计算日期，并将结果输出到串行监视器：
```Arduino
#include <Time.h>
#include <TimeLib.h>

int day, month, year;

void setup() {
  Serial.begin(9600);
  setTime(20, 25, 00, 1, 1, 2020); // 设置时间为2020年1月1日20时25分00秒
  day = day(); // 获取当前日期的天数
  month = month(); // 获取当前日期的月份
  year = year(); // 获取当前日期的年份
  Serial.print("今天是");
  Serial.print(year); // 输出年份
  Serial.print("-");
  Serial.print(month); // 输出月份
  Serial.print("-");
  Serial.println(day); // 输出天数

  tmElements_t futureDate; // 定义一个时间结构体
  futureDate.Second = 0; // 设置秒数为0
  futureDate.Minute = 0; // 设置分钟为0
  futureDate.Hour = 0; // 设置小时为0
  futureDate.Day = 14; // 设置日期为14
  futureDate.Month = 2; // 设置月份为2
  futureDate.Year = 2021; // 设置年份为2021
  time_t futureEpoch = makeTime(futureDate); // 将时间结构体转换为time_t类型
  setTime(futureEpoch); // 设置当前时间为未来日期
  day = day(); // 获取日期的天数
  month = month(); // 获取日期的月份
  year = year(); // 获取日期的年份
  Serial.print("未来日期是");
  Serial.print(year); // 输出年份
  Serial.print("-");
  Serial.print(month); // 输出月份
  Serial.print("-");
  Serial.println(day); // 输出天数
}

void loop() {

}
```

这段代码将输出当前日期和未来日期，您可以根据您的需求修改futureDate结构体来计算其他日期。

## 深入了解Arduino计算日期
Arduino使用Time库来处理日期和时间。该库提供了许多有用的函数和结构体来处理日期和时间。

在本例中，我们使用makeTime函数将日期和时间转换为time_t类型，这样我们就可以使用setTime函数来设置当前日期和时间。

另外，您还可以使用其他函数来计算某一日期之后或之前的日期，例如使用addTime函数来计算未来日期，并使用subtractTime函数来计算过去日期。

## 参考链接
- [Arduino Time库文档](https://www.pjrc.com/teensy/td_libs_Time.html)
- [Arduino Time库GitHub仓库](https://github.com/PaulStoffregen/Time)