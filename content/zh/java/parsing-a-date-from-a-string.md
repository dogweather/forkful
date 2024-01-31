---
title:                "从字符串解析日期"
date:                  2024-01-20T15:36:39.897082-07:00
html_title:           "Arduino: 从字符串解析日期"
simple_title:         "从字符串解析日期"

tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/java/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why? 什么 & 为什么?
解析日期就是将文本字符串转换为日期对象。程序员这么做，是为了能够以更有意义的方式操作和存储日期数据。

## How to: 如何操作：
```java
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.Locale;

public class DateParser {
    public static void main(String[] args) {
        String dateString = "2023年04月15日";
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("uuuu年MM月dd日", Locale.CHINA);
        LocalDate date = LocalDate.parse(dateString, formatter);
        System.out.println(date);
    }
}
```
输出样例： `2023-04-15`

## Deep Dive 深入探究
以前，Java使用`java.util.Date`和`SimpleDateFormat`类来解析和格式化日期。但这些类不够简便，且存在线程安全问题。Java 8引入了`java.time`包，推出了更强大、不易出错的日期时间API，例如`LocalDate`和`DateTimeFormatter`。

除`DateTimeFormatter`外，你还可以使用`java.text.SimpleDateFormat`，但不推荐。国际化方面，`DateTimeFormatter` 支持多种语言环境，易于本地化。

解析细节方面，`DateTimeFormatter`的`ofPattern`方法允许你定义自己的日期格式。处理不同语言环境时，可以传入`Locale` 对象。

## See Also 参考链接
- [DateTimeFormatter 官方文档](https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html)
- [LocalDate 官方文档](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)
- [Oracle Java 教程——日期时间](https://docs.oracle.com/javase/tutorial/datetime/)
