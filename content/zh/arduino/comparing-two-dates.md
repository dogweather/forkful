---
title:                "Arduino: 比较两个日期"
programming_language: "Arduino"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/comparing-two-dates.md"
---

{{< edit_this_page >}}

为什么：在编程中，比较两个日期是非常常见的。它可以帮助我们判断事件发生的先后顺序，计算时间差等等。因此，学习如何比较两个日期是非常必要的。

怎么做：要比较两个日期，我们需要使用Arduino中的日期库。首先，我们需要声明两个日期对象，并将它们分别赋值为要比较的日期。然后，我们可以使用比较运算符来比较这两个日期对象。例如：

```Arduino
#include <DateTime.h>

DateTime date1 = DateTime(2021, 5, 10); //第一个日期对象
DateTime date2 = DateTime(2021, 5, 12); //第二个日期对象

if (date1 < date2) { //使用小于运算符比较两个日期
  Serial.println("Date1 is before Date2.");

} else if (date1 > date2) { //使用大于运算符比较两个日期
  Serial.println("Date1 is after Date2.");

} else { //如果两个日期相同
  Serial.println("Date1 is equal to Date2.");
}

```

输出：Date1 is before Date2.

这里我们使用了DateTime库中的小于运算符“<”和大于运算符“>”来比较两个日期。同样，我们也可以使用等于运算符“==”来判断两个日期是否相同。除此之外，DateTime库还提供了其他一些方法来比较日期，比如比较年份、月份、日期等等。具体使用方法可以参考官方文档。

深入了解：在Arduino中，日期是以Unix时间戳的形式表示的，它代表了从1970年1月1日起经过的秒数。因此，在比较日期时，本质上是在比较这两个日期对应的时间戳。同时，我们也可以通过日期对象的toString()方法来将日期格式化为特定的字符串格式，以便于输出和比较。

另外，如果需要对日期进行加减运算，我们可以使用DateTime库中的add()和subtract()方法。比如，将某个日期加上一定的天数，或者计算两个日期之间相差的天数等等。

最后，需要注意的是，Arduino的日期库并不支持闰年的判断，因此在使用时需要自行进行处理。

参考链接：https://www.arduino.cc/en/Reference/DateTime

参考代码：https://github.com/PaulStoffregen/DateTime

## 参考链接

1. 官方文档：https://www.arduino.cc/en/Reference/DateTime
2. 代码示例：https://github.com/PaulStoffregen/DateTime