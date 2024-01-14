---
title:    "Java: 将日期转换为字符串"
keywords: ["Java"]
---

{{< edit_this_page >}}

# 为什么要将日期转换为字符串

Java中的日期和时间是以特定的格式存储的，可能不太易读。因此，当我们需要在字符串中显示日期时，就需要进行格式转换。这样可以使得日期更易读，更符合我们的需求。

## 如何实现

在Java中，我们可以使用SimpleDateFormat类来将日期转换为字符串。首先，我们需要创建一个SimpleDateFormat对象，指定日期的格式。例如，我们可以使用“yyyy年MM月dd日”来表示日期，其中yyyy表示年份，MM表示月份，dd表示日期。然后，我们使用format方法来将日期转换为字符串。

```Java
String pattern = "yyyy年MM月dd日";
SimpleDateFormat sdf = new SimpleDateFormat(pattern);
Date date = new Date();
String dateStr = sdf.format(date);
System.out.println("当前日期为：" + dateStr);
```

执行以上代码，输出为：当前日期为：2021年10月01日。可以看到，日期已经被转换为指定的格式的字符串。

## 深入了解

日期转换为字符串时，有一些常用的格式符号习惯用法，以下是一些常用的格式符号：

- yyyy：4位纯数字年份，例如2021表示为2021
- yy：2位纯数字年份，例如2021表示为21
- MM：2位数值的月份，例如9月份表示为09
- M：1位数值的月份，例如9月份表示为9
- dd：2位数值的日期，例如1号表示为01
- d：1位数值的日期，例如1号表示为1
- hh：12小时制的小时数，例如下午3点表示为03
- HH：24小时制的小时数，例如下午3点表示为15
- mm：分钟数，例如20分钟表示为20
- ss：秒数，例如30秒表示为30

另外，日期转换为字符串时还需考虑时区，以及日期的本地化显示。

# 请参考

- SimpleDateFormat类文档：https://docs.oracle.com/javase/8/docs/api/java/text/SimpleDateFormat.html
- Java日期和时间格式符号参考：https://docs.oracle.com/javase/7/docs/api/java/text/SimpleDateFormat.html
- SimpleDateFormat教程：https://www.runoob.com/java/java-date-time.html