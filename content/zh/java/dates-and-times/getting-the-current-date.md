---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:09:52.062134-07:00
description: "\u5728Java\u4E2D\u83B7\u53D6\u5F53\u524D\u65E5\u671F\u662F\u4E00\u4E2A\
  \u57FA\u7840\u64CD\u4F5C\uFF0C\u5B83\u5141\u8BB8\u7A0B\u5E8F\u5458\u64CD\u4F5C\u65E5\
  \u671F\u5BF9\u8C61\u4EE5\u8FDB\u884C\u65E5\u5FD7\u8BB0\u5F55\u3001\u65E5\u671F\u8BA1\
  \u7B97\u548C\u57FA\u4E8E\u65F6\u95F4\u7684\u6761\u4EF6\u7B49\u64CD\u4F5C\u3002\u5728\
  \u9700\u8981\u8FFD\u8E2A\u3001\u8C03\u5EA6\u548C\u65F6\u95F4\u6570\u636E\u5206\u6790\
  \u7684\u5E94\u7528\u7A0B\u5E8F\u4E2D\u8FD9\u4E00\u70B9\u81F3\u5173\u91CD\u8981\u3002"
lastmod: '2024-03-13T22:44:47.636876-06:00'
model: gpt-4-0125-preview
summary: "\u5728Java\u4E2D\u83B7\u53D6\u5F53\u524D\u65E5\u671F\u662F\u4E00\u4E2A\u57FA\
  \u7840\u64CD\u4F5C\uFF0C\u5B83\u5141\u8BB8\u7A0B\u5E8F\u5458\u64CD\u4F5C\u65E5\u671F\
  \u5BF9\u8C61\u4EE5\u8FDB\u884C\u65E5\u5FD7\u8BB0\u5F55\u3001\u65E5\u671F\u8BA1\u7B97\
  \u548C\u57FA\u4E8E\u65F6\u95F4\u7684\u6761\u4EF6\u7B49\u64CD\u4F5C\u3002\u5728\u9700\
  \u8981\u8FFD\u8E2A\u3001\u8C03\u5EA6\u548C\u65F6\u95F4\u6570\u636E\u5206\u6790\u7684\
  \u5E94\u7528\u7A0B\u5E8F\u4E2D\u8FD9\u4E00\u70B9\u81F3\u5173\u91CD\u8981\u3002."
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
