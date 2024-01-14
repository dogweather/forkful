---
title:    "Arduino: 将日期转换为字符串"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 为什么

首先，让我们来思考一下为什么我们需要将日期转换为字符串。在编程中，日期和时间通常以数字的形式存储，例如2021年7月1日。但是，当我们需要将它们显示给用户时，我们通常希望以友好的日期格式来呈现，例如“2021年7月1日”。因此，将日期转换为字符串可以让我们更方便地控制如何显示日期，并使用户更容易理解。

## 如何做

在Arduino编程中，日期和时间都是通过标准库中的Time库来处理的。要将日期转换为字符串，首先我们需要将日期存储在一个tmElements_t结构中，该结构包含日期和时间的各个成员变量。例如，我们想将当前日期转换为字符串，我们可以这样做：

```Arduino
tmElements_t now;
now.Year = 2021;
now.Month = 7;
now.Day = 1;
```

接下来，我们使用函数`makeDateString()`来将tmElements_t结构中的日期转换为字符串。该函数接受四个参数：tmElements_t结构，日期格式，日期分隔符和是否包含星期几。例如，我们想将2021年7月1日转换为“2021-07-01（周四）”，我们可以这样做：

```Arduino
String dateString = makeDateString(now, "YYYY-MM-DD (DDD)");
```

最后，我们可以使用`print()`函数来显示字符串日期：

```Arduino
Serial.println(dateString);
```

输出将是：“2021-07-01（周四）”。

## 深入探讨

在`makeDateString()`函数中，我们使用了C++的格式化字符串函数`strftime()`来实现日期格式的转换。该函数非常强大，可以根据我们传入的格式字符串来显示不同的日期格式。例如，我们可以使用“YY年第D天（DDD）”，将日期格式转换为“21年第182天（周四）”。

值得注意的是，在Arduino中使用`strftime()`函数需要首先在sketch中添加`#include <TimeLib.h>`语句来包含Time库。

## 参考链接

- [Time Library - Arduino](https://www.arduino.cc/reference/en/libraries/time/)
- [C++ String Formatting - cppreference.com](https://en.cppreference.com/w/cpp/io/c/fprintf)
- [Strftime Functions - cppreference.com](https://en.cppreference.com/w/cpp/chrono/c/strftime)

---

## 参见

- [显示当前日期和时间 - 文档中心](https://www.arduino.cc/en/hacking/library-examples/time)