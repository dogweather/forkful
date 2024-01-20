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

## 是什么和为什么？

未来或过去的日期计算是通过编写代码方式，处理和预测日历的一种技巧。程序员这么做，主要是为了方便管理和操作日历数据。

## 如何操作：

以下是一段简单的Arduino代码，展示如何计算未来的日期：

```Arduino
#include <TimeLib.h> 

void setup() {
  setTime(14, 0, 0, 28, 10, 2022); //设置初始时间与日期
}

void loop() {
  time_t futureTime = now() + (7 * DAYS); //计算未来一周的日期（7天后）

  Serial.print(hour(futureTime));
  Serial.print(":");
  Serial.print(minute(futureTime));
  Serial.print(":");
  Serial.print(second(futureTime));
  Serial.print(" ");
  Serial.print(day(futureTime));
  Serial.print(".");
  Serial.print(month(futureTime));
  Serial.print(".");
  Serial.println(year(futureTime)); 

  delay(1000);
}
```

此示例将显示当前日期向后推一周的时间和日期。

## 深度探索：

在计算机编程早期，计算未来或过去的日期着实是个挑战，因为电脑资源有限。现在，有了多种库和函数（如TimeLib），完成这种任务就变得更加容易了。

除了Arduino，其他语言（如Python，JavaScript等）也有解决相同问题的不同方法。例如，Python有一个被广泛应用的库叫做dateutils。

在Arduino中使用TimeLib库来计算未来或过去的日期，本质上是通过`now()`函数取得当前的系统时间（Unix时间戳格式），然后通过特定的算法（加或减秒数）来计算未来或过去的某个时间点。

## 另请参见：

如果想要查看关于日期和时间处理的更多信息，请参考以下资源：

- [Arduino官方文档](https://www.arduino.cc/reference/en/)
- [TimeLib库](https://www.pjrc.com/teensy/td_libs_Time.html)
- [Python Dateutils库官网](https://dateutil.readthedocs.io/en/stable/)
- [JavaScript Date 对象详解](https://www.runoob.com/js/js-obj-date.html)