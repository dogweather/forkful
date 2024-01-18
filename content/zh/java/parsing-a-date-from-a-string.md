---
title:                "从字符串解析日期"
html_title:           "Java: 从字符串解析日期"
simple_title:         "从字符串解析日期"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/java/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## 什么是日期字符串解析? 
日期字符串解析是将一个日期字符串（如“2021年10月15日”）转换成一个特定格式的日期对象的过程。程序员通常需要这么做是因为他们需要将日期字符串转换为日期对象以便在程序中使用。

## 如何进行日期字符串解析: 
Java提供了一个方便的工具来进行日期字符串解析，即`SimpleDateFormat`类。以下是一个简单的示例代码，展示了如何使用该类进行日期字符串解析。 

```Java 
import java.text.SimpleDateFormat;
import java.util.Date;

public class DateParsingExample {

    public static void main(String[] args) {
        String dateString = "2021年10月15日";
        SimpleDateFormat format = new SimpleDateFormat("yyyy年MM月dd日");
        try {
            Date date = format.parse(dateString);
            System.out.println(date); // 输出：Fri Oct 15 00:00:00 CST 2021
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
```

## 深入了解: 
日期字符串解析的历史可以追溯到计算机的早期。在过去，程序员必须手动解析日期字符串，这导致了很多错误。现在，Java的`SimpleDateFormat`类使得日期字符串解析变得简单方便。除了`SimpleDateFormat`类，还有其他一些解析日期字符串的工具，如Joda-Time和Java 8的DateTimeFormatter类。

## 参考链接: 
- [Java Date Formatting and Parsing with SimpleDateFormatter](https://www.baeldung.com/java-string-to-date)
- [Joda-Time官方文档](https://www.joda.org/joda-time/)
- [DateTimeFormatter官方文档](https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html)