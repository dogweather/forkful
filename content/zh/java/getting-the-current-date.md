---
title:                "获取当前日期"
date:                  2024-01-20T15:15:00.629379-07:00
simple_title:         "获取当前日期"

tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/java/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？
获取当前日期意味着让程序能知道现在是什么时候。开发人员需要这样做来记录事件、处理日期相关的逻辑，或是给用户显示当前时间。

## 如何操作：
```java
import java.time.LocalDate;

public class Main {
    public static void main(String[] args) {
        LocalDate currentDate = LocalDate.now();
        System.out.println("当前日期是：" + currentDate);
    }
}
```
输出样例：
```
当前日期是：2023-04-05
```

## 深入探索
获取当前日期的功能从Java 8开始就大为简化了，推出了java.time包，这是基于Joda-Time库改进的。历史上，程序员需要依赖java.util.Date或java.util.Calendar，这两者都不够直观且容易出错。使用LocalDate等现代API，不仅代码可读性更好，而且线程安全且不可变。

替代方案主要包括使用老旧的java.util.Date和java.util.Calendar类，或者第三方库如Joda-Time。不过，自从java.time的引入，推荐尽可能使用这个库来简化和统一日期时间操作。

如果需要时区支持，可以使用java.time.ZonedDateTime。java.time包下的类准守ISO-8601标准，并且提供了诸多日期和时间的操作方法，例如加减天数、比较日期等。

## 参考连接
- [Oracle官方教程 - Date Time](https://docs.oracle.com/javase/tutorial/datetime/)
- [Java 8 java.time包概述](https://www.oracle.com/technical-resources/articles/java/jf14-date-time.html)
- [Java LocalDate类文档](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)
