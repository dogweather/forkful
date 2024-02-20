---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:14:18.406249-07:00
description: "\u4ECE\u5B57\u7B26\u4E32\u89E3\u6790\u65E5\u671F\u6D89\u53CA\u5C06\u65E5\
  \u671F\u548C\u65F6\u95F4\u7684\u6587\u672C\u8868\u793A\u5F62\u5F0F\u8F6C\u6362\u4E3A\
  `Date`\u5BF9\u8C61\u6216\u66F4\u73B0\u4EE3\u7684`LocalDateTime`\u5BF9\u8C61\u3002\
  \u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\u662F\u4E3A\u4E86\u64CD\u4F5C\u3001\u683C\u5F0F\
  \u5316\u3001\u6BD4\u8F83\u6216\u4EE5\u6807\u51C6\u5316\u683C\u5F0F\u5B58\u50A8\u65E5\
  \u671F\uFF0C\u8FD9\u5BF9\u4E8E\u9700\u8981\u65E5\u671F\u8BA1\u7B97\u3001\u9A8C\u8BC1\
  \u6216\u4E00\u81F4\u7684\u56FD\u9645\u5316\u7684\u5E94\u7528\u7A0B\u5E8F\u81F3\u5173\
  \u91CD\u8981\u3002"
lastmod: 2024-02-19 22:05:06.660242
model: gpt-4-0125-preview
summary: "\u4ECE\u5B57\u7B26\u4E32\u89E3\u6790\u65E5\u671F\u6D89\u53CA\u5C06\u65E5\
  \u671F\u548C\u65F6\u95F4\u7684\u6587\u672C\u8868\u793A\u5F62\u5F0F\u8F6C\u6362\u4E3A\
  `Date`\u5BF9\u8C61\u6216\u66F4\u73B0\u4EE3\u7684`LocalDateTime`\u5BF9\u8C61\u3002\
  \u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\u662F\u4E3A\u4E86\u64CD\u4F5C\u3001\u683C\u5F0F\
  \u5316\u3001\u6BD4\u8F83\u6216\u4EE5\u6807\u51C6\u5316\u683C\u5F0F\u5B58\u50A8\u65E5\
  \u671F\uFF0C\u8FD9\u5BF9\u4E8E\u9700\u8981\u65E5\u671F\u8BA1\u7B97\u3001\u9A8C\u8BC1\
  \u6216\u4E00\u81F4\u7684\u56FD\u9645\u5316\u7684\u5E94\u7528\u7A0B\u5E8F\u81F3\u5173\
  \u91CD\u8981\u3002"
title: "\u4ECE\u5B57\u7B26\u4E32\u89E3\u6790\u65E5\u671F"
---

{{< edit_this_page >}}

## 什么 & 为什么？
从字符串解析日期涉及将日期和时间的文本表示形式转换为`Date`对象或更现代的`LocalDateTime`对象。程序员这样做是为了操作、格式化、比较或以标准化格式存储日期，这对于需要日期计算、验证或一致的国际化的应用程序至关重要。

## 如何操作：

### 使用`java.time`包（在Java 8及以后版本推荐）:
```java
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;

public class DateParser {
    public static void main(String[] args) {
        String dateString = "2023-04-30";
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd");
        LocalDate date = LocalDate.parse(dateString, formatter);
        System.out.println(date); // 输出: 2023-04-30
    }
}
```

### 使用`SimpleDateFormat`（较旧的方法）:
```java
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;

public class DateParser {
    public static void main(String[] args) {
        String dateString = "30/04/2023";
        SimpleDateFormat formatter = new SimpleDateFormat("dd/MM/yyyy");
        try {
            Date date = formatter.parse(dateString);
            System.out.println(date); // 输出格式依赖于您的系统默认格式
        } catch (ParseException e) {
            e.printStackTrace();
        }
    }
}
```

### 使用第三方库（例如，Joda-Time）:
由于在Java 8中引入了`java.time`包，Joda-Time作为一个重要的第三方库目前处于维护模式。然而，对于使用Java 8之前版本的用户，Joda-Time是一个不错的选择。
```java
import org.joda.time.LocalDate;
import org.joda.time.format.DateTimeFormat;
import org.joda.time.format.DateTimeFormatter;

public class DateParser {
    public static void main(String[] args) {
        String dateString = "2023-04-30";
        DateTimeFormatter formatter = DateTimeFormat.forPattern("yyyy-MM-dd");
        LocalDate date = LocalDate.parse(dateString, formatter);
        System.out.println(date); // 输出: 2023-04-30
    }
}
```
注意，在处理日期时，如果解析或格式化日期时间而不仅仅是日期，始终要注意时区设置。
