---
title:                "获取当前日期"
aliases:
- zh/java/getting-the-current-date.md
date:                  2024-02-03T19:09:52.062134-07:00
model:                 gpt-4-0125-preview
simple_title:         "获取当前日期"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/java/getting-the-current-date.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 什么和为什么？
在Java中获取当前日期是一个基础操作，它允许程序员操作日期对象以进行日志记录、日期计算和基于时间的条件等操作。在需要追踪、调度和时间数据分析的应用程序中这一点至关重要。

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
