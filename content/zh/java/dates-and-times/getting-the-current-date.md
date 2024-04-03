---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:09:52.062134-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A Java\u63D0\u4F9B\u4E86\u591A\u79CD\u65B9\
  \u5F0F\u6765\u83B7\u53D6\u5F53\u524D\u65E5\u671F\uFF0C\u65E2\u5305\u62EC\u8001\u65E7\
  \u7684`java.util.Date`\u7C7B\uFF0C\u4E5F\u5305\u62EC\u5728Java 8\u4E2D\u5F15\u5165\
  \u3001\u66F4\u52A0\u591A\u529F\u80FD\u548C\u76F4\u89C2\u7684\u65B0`java.time`\u5305\
  \u3002 #."
lastmod: '2024-03-13T22:44:47.636876-06:00'
model: gpt-4-0125-preview
summary: "Java\u63D0\u4F9B\u4E86\u591A\u79CD\u65B9\u5F0F\u6765\u83B7\u53D6\u5F53\u524D\
  \u65E5\u671F\uFF0C\u65E2\u5305\u62EC\u8001\u65E7\u7684`java.util.Date`\u7C7B\uFF0C\
  \u4E5F\u5305\u62EC\u5728Java 8\u4E2D\u5F15\u5165\u3001\u66F4\u52A0\u591A\u529F\u80FD\
  \u548C\u76F4\u89C2\u7684\u65B0`java.time`\u5305."
title: "\u83B7\u53D6\u5F53\u524D\u65E5\u671F"
weight: 29
---

## 如何操作：
Java提供了多种方式来获取当前日期，既包括老旧的`java.util.Date`类，也包括在Java 8中引入、更加多功能和直观的新`java.time`包。

### 使用`java.time.LocalDate`
```java
import java.time.LocalDate;

public class CurrentDateExample {
    public static void main(String[] args) {
        LocalDate currentDate = LocalDate.now();
        System.out.println(currentDate); // 示例输出：2023-04-01
    }
}
```

### 使用`java.time.LocalDateTime`
```java
import java.time.LocalDateTime;

public class CurrentDateExample {
    public static void main(String[] args) {
        LocalDateTime currentDateTime = LocalDateTime.now();
        System.out.println(currentDateTime); // 示例输出：2023-04-01T12:34:56.789
    }
}
```

### 使用`java.util.Date`（遗留）
```java
import java.util.Date;

public class CurrentDateExample {
    public static void main(String[] args) {
        Date currentDate = new Date();
        System.out.println(currentDate); // 示例输出：周六 4月 01 12:34:56 BST 2023
    }
}
```

### 使用第三方库：Joda-Time
在Java 8之前，Joda-Time是Java中日期和时间的事实标准。如果您在处理遗留系统，或者偏好使用Joda-Time，以下是如何使用它来获取当前日期的方法：
```java
import org.joda.time.LocalDate;

public class CurrentDateExample {
    public static void main(String[] args) {
        LocalDate currentDate = LocalDate.now();
        System.out.println(currentDate); // 示例输出：2023-04-01
    }
}
```
**注意：** 虽然`java.util.Date`和Joda-Time仍在使用，但鉴于其不可变性和全面的API用于处理日期和时间，`java.time`包被推荐用于新项目。
