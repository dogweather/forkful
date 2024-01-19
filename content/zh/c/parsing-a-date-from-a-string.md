---
title:                "从字符串解析日期"
html_title:           "C: 从字符串解析日期"
simple_title:         "从字符串解析日期"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## 什么 & 为什么?

字符串中的日期解析是从文本字符串中分析和提取日期信息的过程。程序员之所以要进行这个过程，是因为在不同的操作系统、应用程序和网络协议中，日期都以不同的字符串格式表示。

## 如何操作:

下面是用C编程语言解析字符串日期的简单示例:

``` C
#include <time.h>
#include <stdio.h>

void parse_date(char *date_str) {
   struct tm tm;
   if (strptime(date_str, "%Y-%m-%d", &tm)) {
       printf("Year: %d; Month: %d; Day: %d\n", tm.tm_year + 1900, tm.tm_mon + 1, tm.tm_mday);
   } else {
       printf("Invalid date!\n");
   }
}

int main(void) {
   parse_date("2022-07-31");
   return 0;
}
```

输出将如下所示:

```
Year: 2022; Month: 7; Day: 31
```

## 深度探究

1. **历史背景**: 从字符串解析日期的方法起源于早期的编程语言实现，包括表处理器和打印编程语言的编译器。
 
2. **替代方法**: 在C++、Java和Python等语言中，都有自己解析日期字符串的方法。

3. **实施细节**: 在C中, `strptime`函数用于解析字符串中的日期和时间信息。它会尝试根据提供的格式字符串来解析输入字符串。如果解析成功，该函数将时间结构体的相应字段设置为解析出的值。

## 另请参阅

- [C库函数 - strptime()](https://www.tutorialspoint.com/c_standard_library/c_function_strptime.htm)

- [C datetime函数库](http://www.cplusplus.com/reference/ctime/)

- [C语言如何将字符串转化为时间](https://stackoverflow.com/questions/321849/strptime-equivalent-in-windows-c-libraries)