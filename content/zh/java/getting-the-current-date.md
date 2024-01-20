---
title:                "获取当前日期"
html_title:           "Arduino: 获取当前日期"
simple_title:         "获取当前日期"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/java/getting-the-current-date.md"
---

{{< edit_this_page >}}

# 获取当前日期: Java 如何做到？

## 什么和为什么？

获取当前日期意味着在程序中捕获并使用当前的日期和时间。这对于记录事件，跟踪时间和进行时间相关计算非常有用。

## 如何实现：

在 Java 中获取当前日期的基本代码如下：

```Java
import java.time.LocalDate;

public class Main {
    public static void main(String[] args) {
        LocalDate date = LocalDate.now();
        System.out.println("当前日期为: " + date);
    }
}
```

运行上述代码将输出当前日期，如下所示：

```Java
当前日期为: 2022-09-15
```

## 深入探究：

在 Java 推出 java.time 包之前，我们通常使用 java.util.Date 类来获取当前日期。然而，由于 Date 类既想表示日期又想表示时间，导致了许多设计上的问题。

这就使得使用 LocalDate 类来只获取日期更有优势。此外，`LocalDate.now()` 方法默认使用系统时钟的日期。

另一个选择是 java.util.Calendar 类，它比 Date 类提供更多的功能，但使用起来较为复杂。

## 更多参考：

* [Java 8日期和时间API](https://www.baeldung.com/java-8-date-time-intro)
* [Java LocalDate 文档](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)
* [Java Date vs Calendar](https://www.baeldung.com/java-date-vs-calendar)