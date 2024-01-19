---
title:                "从字符串解析日期"
html_title:           "C: 从字符串解析日期"
simple_title:         "从字符串解析日期"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## 什么以及为什么?
解析字符串中的日期意味着将字符串转化为程序可以理解的日期格式。程序员之所以要这么做，是因为这能帮助我们正确处理和操作日期数据。

## 如何做：
在Arduino中，我们可以使用内建函数，来解析字符串中的日期。例如：

```Arduino
#include <TimeLib.h>

time_t t;
char dateStr [] = "2022-07-22 19:30:01";

void setup() {
  Serial.begin(9600);
  t = parseTime(dateStr);
  if (t == 0) {
    Serial.println("Failed to parse time");
  } else {
    Serial.println("Time parsed successfully");
  }
}

time_t parseTime(char* dateTime) {
  char *dateToken;
  int year, month, day, hour, minute, second;

  dateToken = strtok(dateTime, "-");
  year = atoi(dateToken);

  dateToken = strtok(NULL, "-");
  month = atoi(dateToken);
  
  dateToken = strtok(NULL, " ");
  day = atoi(dateToken);
 
  dateToken = strtok(NULL, ":");
  hour = atoi(dateToken);

  dateToken = strtok(NULL, ":");
  minute = atoi(dateToken);

  dateToken = strtok(NULL, "\0");
  second = atoi(dateToken);

  return makeTime(second, minute, hour, day, month, year);
}
```

当你运行这段代码，Arduino代码就会从`dateStr`字符串中解析出日期和时间，并打印出成功的消息。

## 深入探讨
解析字符串日期有着悠久的历史，但Arduino直到最近才发展出强大的库支持。除了`<TimeLib.h>`，还有其他库如`<DateTime.h>`可供选择。 需要注意的是，使用不同的库，方法名和参数可能会有所不同，但基本概念是相同的。 

日期解析的具体实现中涉及到了`strtok`和`atoi`这两个重要的函数。`strtok`函数用于分割字符串，而`atoi`函数则用于将字符串转换为整数。

## 参考文献
为了提高你的Arduino编程技能，你可能会对以下资源有兴趣：
1. Arduino 官方网站: <https://www.arduino.cc/>
2. Arduino中文社区论坛: <http://www.arduino.cn/>
3. 《Getting Started with Arduino》
4. 《Arduino Cookbook》