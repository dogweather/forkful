---
date: 2024-01-20 17:36:57.264710-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u4F7F\u7528Java8\u7684DateTimeFormatter."
isCJKLanguage: true
lastmod: '2024-04-05T22:38:46.797876-06:00'
model: gpt-4-1106-preview
summary: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u4F7F\u7528Java8\u7684DateTimeFormatter."
title: "\u5C06\u65E5\u671F\u8F6C\u6362\u4E3A\u5B57\u7B26\u4E32"
weight: 28
---

## 如何操作：
```java
import java.text.SimpleDateFormat;
import java.util.Date;

public class DateFormatterExample {
    public static void main(String[] args) {
        // 创建一个日期实例
        Date date = new Date();
        // 创建SimpleDateFormat实例并定义格式
        SimpleDateFormat formatter = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
        // 将日期转换成字符串
        String strDate = formatter.format(date);
        // 输出格式化后的字符串
        System.out.println(strDate);
        // 示例输出: 2023-04-06 14:20:35
    }
}
```

使用Java8的DateTimeFormatter:
```java
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;

public class DateTimeFormatterExample {
    public static void main(String[] args) {
        // 创建一个LocalDateTime实例
        LocalDateTime now = LocalDateTime.now();
        // 创建一个DateTimeFormatter并定义格式
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss");
        // 将日期时间转换成字符串
        String formattedDate = now.format(formatter);
        // 输出格式化后的字符串
        System.out.println(formattedDate);
        // 示例输出: 2023-04-06 14:22:10
    }
}
```

## 深入探讨：
日期到字符串的转换可以追溯到Java早期，当时由`java.text.SimpleDateFormat`类实现。随着时间的推移，Java 8引入了`java.time`包（称为JSR-310），提供了更好的日期和时间API，如`DateTimeFormatter`，来代替旧的`SimpleDateFormat`。新的API提供了不变性和线程安全性，同时修复了旧API的设计问题。

使用`SimpleDateFormat`存在线程安全问题，而推荐的`DateTimeFormatter`则不存在这个问题。另外，`DateTimeFormatter`支持更多的ISO和自定义的日期格式。

记住这一点，选择正确的API，取决于你的使用场景及Java版本。

## 另请参见：
- [Java SimpleDateFormat官方文档](https://docs.oracle.com/javase/8/docs/api/java/text/SimpleDateFormat.html)
- [Java DateTimeFormatter官方文档](https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html)
- [JSR 310: Date and Time API](https://jcp.org/en/jsr/detail?id=310)
