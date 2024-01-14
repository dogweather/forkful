---
title:                "Swift: 计算未来或过去的日期"
programming_language: "Swift"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/swift/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# 为什么要计算未来或过去的日期（Why）

计算未来或过去的日期可能是一个常见的需求，例如在日历应用中设置提醒或在倒数日应用中显示剩余的天数。无论何种情况，了解如何在Swift中编写代码来计算未来或过去的日期都是非常有用的。

# 如何计算未来或过去的日期（How To）

要计算未来或过去的日期，您可以使用Swift中的`Date`和`Calendar`类。下面是一个简单的示例，它展示了如何计算明天的日期：

```Swift
let tomorrow = Calendar.current.date(byAdding: .day, value: 1, to: Date())
print(tomorrow) // Output: Optional(2021-07-27 01:53:07 +0000)
```

在这个示例中，我们使用了`Calendar`类的`date(byAdding:value:to:)`方法来将当前日期加一天。这个方法接受三个参数：第一个参数是要添加的时间间隔，第二个参数是要添加的值，第三个参数是要添加到的日期。这里我们使用了`.day`作为时间间隔，并将值设为1，表示要加一天。最后，我们使用`print`函数来输出计算出的日期。

你也可以根据自己的需求来计算其他的未来或过去的日期，比如计算未来一周的日期：

```Swift
let nextWeek = Calendar.current.date(byAdding: .day, value: 7, to: Date())
print(nextWeek) // Output: Optional(2021-08-02 01:53:07 +0000)
```

同样的，你也可以使用负值来计算过去的日期，比如计算5天前的日期：

```Swift
let fiveDaysAgo = Calendar.current.date(byAdding: .day, value: -5, to: Date())
print(fiveDaysAgo) // Output: Optional(2021-07-22 01:53:07 +0000)
```

# 深入了解计算未来或过去的日期（Deep Dive）

要更深入地了解如何计算未来或过去的日期，你需要了解`Date`和`Calendar`类的相关知识。`Date`类表示一个特定的日期和时间，而`Calendar`类则用于处理日期和时间的计算。Swift中也提供了`DateComponents`类，它可以帮助我们更容易地处理日期和时间的各个部分，比如年、月、日等等。

在上面的例子中，我们使用了`Calendar`类的`current`属性来获取系统当前的日历。你也可以根据需要自己创建一个`Calendar`对象，比如根据某个国家的习惯来计算日期。同时，`Date`类也有许多内置的方法可以帮助我们处理日期和时间，比如`addingTimeInterval(_:)`方法可以让我们在一个日期上加上一个时间间隔。

# 详见（See Also）

- [Apple官方文档：日期和时间编程指南（英文）](https://developer.apple.com/documentation/foundation/dates_and_times)
- [简书：`Date`和`Calendar`的使用（中文）](https://www.jianshu.com/p/5c1be9b3c4c6)
- [伯乐在线：Swift日期处理（中文）](https://blog.jobbole.com/116536/)