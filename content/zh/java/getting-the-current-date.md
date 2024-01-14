---
title:                "Java: 获取当前日期"
programming_language: "Java"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/java/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 为什么

获取当前日期可能看起来并不是一个很复杂的任务，但它在程序开发中却是非常重要的一步。无论是记录数据还是进行条件判断，都需要获取到正确的当前日期。因此，了解如何在Java中获取当前日期是非常有必要的。

## 如何

要在Java中获取当前日期，需要使用Java内置的`java.util.Date`类。首先，需要导入这个类，然后创建一个新的`Date`对象来存储当前日期。

```Java
import java.util.Date;

// 创建一个当前日期的Date对象
Date currentDate = new Date();

// 打印出当前日期
System.out.println(currentDate);
```

运行上述代码，你将会看到如下输出：

```Java
Tue Apr 20 20:30:25 CST 2021
```

从上面的输出可以看出，`Date`对象存储的是当前日期和时间的完整信息。但是，如果你只需要获取当前日期，可以使用Java中的`SimpleDateFormat`类来进行格式化输出。

```Java
import java.text.SimpleDateFormat;
import java.util.Date;

// 创建一个当前日期的Date对象
Date currentDate = new Date();

// 定义日期的格式
SimpleDateFormat format = new SimpleDateFormat("yyyy-MM-dd");

// 格式化输出当前日期
System.out.println(format.format(currentDate));
```

运行上述代码，你将会得到一个简洁的输出，只显示当前日期的年份、月份和日期。

```Java
2021-04-20
```

## 深入了解

在Java中获取当前日期其实还有一种更方便的方式，就是使用`Calendar`类。这个类提供了很多与日期相关的方法，比如可以设置特定的日期，或者获取特定日期的月份、年份等信息。

```Java
import java.util.Calendar;

// 创建一个Calendar对象
Calendar calendar = Calendar.getInstance();

// 获取当前日期的年份
int year = calendar.get(Calendar.YEAR);

// 获取当前日期的月份
int month = calendar.get(Calendar.MONTH) + 1;

// 获取当前日期的日期
int day = calendar.get(Calendar.DAY_OF_MONTH);

// 输出当前日期
System.out.println(year + "-" + month + "-" + day);
```

运行上述代码，你将会得到与前面相同的输出。

另外，`Calendar`类还可以用来设置特定的日期。比如，如果你需要获取昨天的日期，可以使用`add`方法来设置`Calendar`对象的日期。

```Java
import java.util.Calendar;

// 创建一个Calendar对象
Calendar calendar = Calendar.getInstance();

// 设置日期为昨天
calendar.add(Calendar.DAY_OF_MONTH, -1);

// 输出昨天的日期
System.out.println(calendar.get(Calendar.DAY_OF_MONTH));
```

运行上述代码，你将会得到昨天的日期。

## 参考链接

- [Java获取当前日期的方法](https://www.runoob.com/java/java-date-time.html)
- [Java日期操作指南](https://www.baeldung.com/java-date-time)
- [Java文档：java.util.Date](https://docs.oracle.com/javase/8/docs/api/java/util/Date.html)