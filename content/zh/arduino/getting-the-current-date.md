---
title:    "Arduino: 获取当今日期"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

# 为什么要获取当前日期？

在进行 Arduino 编程时，获取当前日期是一个非常重要的操作。通过获取当前日期，我们可以在项目中使用时间戳、制作时钟和日历应用，或者实现其他需要特定日期信息的功能。此外，获取当前日期还可以帮助我们在项目中跟踪时间流逝，从而优化代码。

## 如何获取当前日期？

要在 Arduino 中获取当前日期，我们可以使用 `RTClib` 库。首先，我们需要将这个库添加到我们的代码中。然后，我们可以通过以下代码来获取当前日期：

```Arduino
#include <Wire.h>
#include "RTClib.h"

RTC_DS1307 rtc;

void setup() {
  Wire.begin();
  rtc.begin();
  if (!rtc.isrunning()) {
    rtc.adjust(DateTime(__DATE__, __TIME__));
  }
}

void loop() {
  DateTime now = rtc.now(); // 获取当前日期和时间
  int year = now.year(); // 获取当前年份
  int month = now.month(); // 获取当前月份
  int day = now.day(); // 获取当前日期
  int hour = now.hour(); // 获取当前小时数
  int minute = now.minute(); // 获取当前分钟数
  int second = now.second(); // 获取当前秒数
  // 在这里可以使用获取的日期信息做更多的操作
  delay(1000); // 每秒更新一次日期
}
```

通过以上代码，我们可以实时获取当前的日期信息，并将其用于后续的操作。

## 深入了解获取当前日期

在 Arduino 中，时间被表示为从某个特定日期开始的毫秒数。这个特定日期被称为"纪元"，通常是1970年1月1日。通过这个日期，我们可以计算出从那一天开始到现在所经过的毫秒数，从而获取到当前日期和时间。

但是，由于 Arduino 的内存有限，不能够存储大量的日期信息。因此，如果我们要在项目中使用日期信息，最好使用外部实时时钟（RTC）模块，如 DS1307 或 DS3231。这些模块可以提供精确的日期和时间信息，并且能够持续运行即使 Arduino 被断电。

另外，我们也可以通过修改 `dt` 变量来设置 Arduino 的时间。例如，我们可以通过以下代码将时间设置为当前日期和时间：

```Arduino
void setDate() {
  rtc.adjust(DateTime(2021, 1, 1, 12, 0, 0)); // 设置日期为2021年1月1日12点
}
```

获取当前日期在 Arduino 编程中是一个常见的操作，也是一个有用的技能。希望通过这篇博客能帮助大家更好地掌握获取当前日期的方法和原理。

# See Also
- [RTClib library](https://github.com/adafruit/RTClib)
- [DS1307 RTC module](https://www.adafruit.com/product/3296)
- [DS3231 RTC module](https://www.adafruit.com/product/3013)