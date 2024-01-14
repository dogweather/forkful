---
title:    "Java: 获取当前日期"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/java/getting-the-current-date.md"
---

{{< edit_this_page >}}

# 为什么要获取当前日期

Java编程中，有时候需要获取当前的日期，以便在程序中使用。可以用于记录创建时间、验证用户登录时间等等。在本文中，我们将介绍如何使用Java代码获取当前日期，并深入探讨这个过程中的细节。

## 如何实现

获取当前日期可以使用`java.util`包中的`Date`类。首先，我们需要在代码的开头导入这个包，然后创建一个`Date`对象。

```Java
import java.util.Date;

// 创建一个Date对象
Date currentDate = new Date();
```

接下来，我们可以通过调用`getDate()`、`getMonth()`、`getYear()`等方法来获取日期的具体信息。

```Java
// 获取日期的日期数，范围为1-31
int day = currentDate.getDate();

// 获取月份，范围为0-11，需要+1
int month = currentDate.getMonth() + 1;

// 获取年份，如2021
int year = currentDate.getYear() + 1900;

// 获取小时数，范围为0-23
int hours = currentDate.getHours();

// 获取分钟数，范围为0-59
int minutes = currentDate.getMinutes();

// 获取秒数，范围为0-59
int seconds = currentDate.getSeconds();
```

以上就是获取当前日期的基本方法。我们可以通过打印这些信息来验证是否正确获取了当前的日期。

```Java
System.out.println("今天是" + year + "年" + month + "月" + day + "日");
System.out.println("现在的时间是" + hours + "点" + minutes + "分" + seconds + "秒");
```

假设今天是2021年5月1日，输出的结果应该为：

今天是2021年5月1日
现在的时间是13点45分32秒

## 深入探讨

在上面的例子中，我们可以看到`getYear()`方法返回的是相对于1900年的年份，这是因为Java中的`Date`类是从Java 1.1版本就存在的，为了向后兼容，这个设计并未改变。如果要获取当前的完整年份，可以使用`Calendar`类。

```Java
import java.util.Calendar;

// 创建一个Calendar对象
Calendar calendar = Calendar.getInstance();

// 获取当前年份
int year = calendar.get(Calendar.YEAR);

// 获取当前月份，范围为0-11，需要+1
int month = calendar.get(Calendar.MONTH) + 1;

// 获取当前日期数，范围为1-31
int day = calendar.get(Calendar.DAY_OF_MONTH);

// 获取当前小时数，范围为0-23
int hours = calendar.get(Calendar.HOUR_OF_DAY);

// 获取当前分钟数，范围为0-59
int minutes = calendar.get(Calendar.MINUTE);

// 获取当前秒数，范围为0-59
int seconds = calendar.get(Calendar.SECOND);
```

另外，`Date`类和`Calendar`类都是不可变对象，即通过调用它们的`set`方法来修改日期是无效的，需要重新创建对象。例如，我们想获取明天的日期，就需要创建一个新的`Date`对象。

```Java
import java.util.Date;

Date currentDate = new Date();

// 使用getTime()方法获取当前日期的时间戳，单位为毫秒
long currentTimeStamp = currentDate.getTime();

// 加上一天的时间，即86400000毫秒，再创建一个newDate对象
Date tomorrow = new Date(currentTimeStamp + 86400000);

// 使用DateFormat类来格式化日期为指定格式
import java.text.DateFormat;
import java.text.SimpleDateFormat;

DateFormat dateFormat = new SimpleDateFormat("yyyy年MM月dd日");
String tomorrowStr = dateFormat.format(tomorrow);

System.out.println("明天是" + tomorrowStr);
```

输出结果为：

明天是2021年5月2日

## 另请参阅

- [Java 8日期时间API教程](http://www.runoob.com/java/java8-datetime-api.html)
- [Java.util.Date文档](https://docs.oracle.com/javase/8/docs/api/java/util/Date.html)
- [Java.util