---
title:    "Kotlin: 获取当前日期"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

# 为什么

在编写程序时，经常需要获取当前的日期。这对于计算机系统管理和数据处理来说非常重要，因为大多数任务都基于特定的日期。使用 Kotlin 可以轻松地获取当前日期，并在程序中使用。

# 如何使用

获取当前日期的最简单方法是使用 `java.util.Date()` 方法。这个方法会返回当前日期和时间的对象，我们可以使用 `toString()` 方法来获取日期的字符串表示形式。下面是一个例子：

```Kotlin
val currentDate = java.util.Date() 
println(currentDate.toString()) // 输出：Mon Jan 25 13:47:19 CST 2021
```

如果你想要获取更加格式化的日期，可以使用 `SimpleDateFormat` 类。这个类允许我们指定日期的显示格式。下面是一个例子：

```Kotlin
val dateFormat = SimpleDateFormat("yyyy-MM-dd")
val currentDate = java.util.Date()
println(dateFormat.format(currentDate)) // 输出：2021-01-25
```

在上面的例子中，我们指定了日期的显示格式为年-月-日。你也可以根据自己的需求指定其他格式，例如月/日/年或是日-月-年等等。

# 深入了解

在上面的例子中，我们使用了 `java.util.Date` 类和 `SimpleDateFormat` 类来获取当前日期。但是，据说在 Kotlin 中使用这些类是不推荐的，因为它们已经过时。相反，我们可以使用 `java.time.LocalDate` 类和 `java.time.format.DateTimeFormatter` 类来获取当前日期。

在使用 `java.time.LocalDate` 类时，我们可以通过 `now()` 方法来获取当前日期，然后使用 `format()` 方法指定日期的显示格式。下面是一个例子：

```Kotlin
val currentDate = LocalDate.now()
println(DateTimeFormatter.ofPattern("dd/MM/yyyy").format(currentDate)) // 输出：25/01/2021
```

使用 `DateTimeFormatter` 类可以让日期格式化更加灵活，你可以根据自己的需要指定不同的日期格式。

# 参考资料

- [Kotlin documentation on Date and Time](https://kotlinlang.org/docs/datetime.html)
- [Official Java documentation on Date and Time](https://docs.oracle.com/javase/8/docs/api/java/util/Date.html)
- [Java documentation on SimpleDateFormat](https://docs.oracle.com/javase/8/docs/api/java/text/SimpleDateFormat.html)
- [Java documentation on LocalDate](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)
- [Java documentation on DateTimeFormatter](https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html)

# 参阅

- [Kotlin 教程](https://kotlinlang.org/docs/getting-started.html)
- [Kotlin 开发者社区](https://kotlinlang.org/community/)