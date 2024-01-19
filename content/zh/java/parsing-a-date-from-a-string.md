---
title:                "从字符串解析日期"
html_title:           "C: 从字符串解析日期"
simple_title:         "从字符串解析日期"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/java/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## 什么和为什么？ (What & Why?)

解析日期是从字符串中提取出具体日期的过程。程序员通常会在处理用户输入或解析外部数据源中的数据时需进行日期解析。

## 如何操作：(How to:)

在Java中，我们可以使用`SimpleDateFormat`类中的`parse()`方法来解析字符串中的日期。看看以下示例：

```Java
import java.text.SimpleDateFormat;
import java.util.Date;

public class Main{

  public static void main(String args[]) throws Exception {
    SimpleDateFormat formatter = new SimpleDateFormat("dd/MM/yyyy");
    String dateInString = "07/06/2023";
    Date date = formatter.parse(dateInString);

    System.out.println(date);
  }
}
```

程序的输出会是：

```Java
Wed Jun 07 00:00:00 CST 2023
```

## 深入探索 (Deep Dive)

### 历史背景
过去，Java程序员使用`java.util.Date`来存储日期和时间，这有时会导致笨重的代码和容易出错。自Java 8引入了新的日期/时间API后，处理日期和时间变得更简单、更直接。

### 替代方案
如果你正在使用Java 8或更高版本，你能使用`java.time.LocalDate`，它更强大而且更易用。以下是使用`LocalDate`解析字符串日期的示例：

```Java
import java.time.LocalDate;

public class Main{

  public static void main(String args[]){
    String date = "2023-06-07";
    LocalDate localDate = LocalDate.parse(date);

    System.out.println(localDate);
  }

}
```

### 实现细节
在使用`java.text.SimpleDateFormat`解析日期时，请小心不要忽略异常。`SimpleDateFormat.parse()`方法声明了可能抛出`ParseException`，必须处理这个异常。

## 参看以下 (See Also)

- Oracle 官方文档，对 Java 日期时间类的详细介绍［链接](https://docs.oracle.com/javase/tutorial/datetime/overview/index.html)
- Baeldung，对 Java 日期字符串解析和格式化的入门教程［链接](https://www.baeldung.com/java-parse-string-to-date)