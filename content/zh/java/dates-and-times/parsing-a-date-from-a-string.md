---
title:                "从字符串解析日期"
aliases:
- zh/java/parsing-a-date-from-a-string.md
date:                  2024-02-03T19:14:18.406249-07:00
model:                 gpt-4-0125-preview
simple_title:         "从字符串解析日期"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/java/parsing-a-date-from-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
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
