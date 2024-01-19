---
title:                "计算未来或过去的日期"
html_title:           "Java: 计算未来或过去的日期"
simple_title:         "计算未来或过去的日期"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/java/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 什麼和為什麼?

計算未來或過去的日期，就是你需要找出距離現在特定天數的日期。為何程序員要這麼做呢？因為這常常用在像是編寫時鐘軟體、處理保質期、或是排程任務等功能。

## 如何做:

在 Java 中我們使用 Calendar 類別來處理時間。以下是一個小例子：

```Java
import java.util.Calendar;

public class Main {
    public static void main(String[] args) {
        Calendar cal = Calendar.getInstance();
       
        // print current date
        System.out.println("今天的日期是: " + cal.getTime());

        // add 5 days to the calendar
        cal.add(Calendar.DATE, 5);
        System.out.println("5 天後的日期是: " + cal.getTime());
    }
}
```

程式輸出將會是：

```
今天的日期是: Mon Apr 12 14:53:45 CST 2021
5 天後的日期是: Sat Apr 17 14:53:45 CST 2021
```

## 深度了解：

在 Java 的早期版本中，我們使用 Date 類別來操作時間。但此類別在許多功能上表現得有些低效且有缺陷，例如並無法支援國際化。因此，在 Java 1.1 版本中，即引入了 Calendar 類別取而代之。

雖然，使用 Calendar 可以很容易地計算未來和過去的日期，但如果你在處理的是大規模的數據，那麼 Joda-Time 這個 Java 庫可能會是較佳的選擇。

另外一種替代方案是 Java 8 之後引入的新的日期時間 API，它更了解任務需求，並提供更豐富的功能來處理日期和時間。

## 另見:

1. [Oracle 官方 Java Calendar 類別文件](https://docs.oracle.com/javase/8/docs/api/java/util/Calendar.html)

2. [Joda-Time - Java date and time library](https://www.joda.org/joda-time/)

3. [Oracle 官方 Java 日期時間 API 導覽](https://docs.oracle.com/javase/tutorial/datetime/index.html)