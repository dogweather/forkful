---
title:                "C#: 获取当前日期"
programming_language: "C#"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c-sharp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 为什么
在编写程序时，经常需要获取当前日期。这对程序的逻辑和功能都非常重要，因此学习如何获取当前日期是十分必要的。

## 如何
获取当前日期在C#中的实现非常简单。我们可以使用DateTime结构中的Now属性来获取当前日期。下面是一个简单的例子：

```C#
var currentDate = DateTime.Now;
Console.WriteLine(currentDate);
```

这段代码首先创建了一个DateTime对象，然后使用Console.WriteLine函数打印出来。在运行程序时，我们会看到当前日期和时间的输出，类似于这样的格式：2021/5/3 21:30:00。

我们也可以使用DateTime结构中的Today属性来获取当前日期的日期部分。下面是一个例子：

```C#
var currentDate = DateTime.Today;
Console.WriteLine(currentDate);
```

这段代码会输出当前日期的日期部分，例如2021/5/3。

除了以上两种方法，我们也可以使用DateTime.Today.ToShortDateString()来获取当前日期的简短字符串表示。同样地，DateTime.Today.ToShortTimeString()可以获取当前日期的简短时间字符串表示。

## 深入了解
在C#中，我们可以使用DateTime结构来表示一个日期和时间的值。它包含了年、月、日、小时、分钟、秒等各种属性，可以满足我们对日期和时间的各种需求。

除了上面提到的方法之外，我们还可以使用DateTime结构中的其他属性来获取当前日期的各个部分。比如通过DateTime.Today.DayOfWeek属性可以获取当前日期是星期几，DateTime.Today.DayOfYear属性可以获取当前日期是今年的第几天等等。

了解DateTime结构的不同属性和方法，有助于我们更加灵活地操作日期和时间，满足不同的需求。

## 参考链接
- [C#中DateTime.Now和DateTime.Today的区别](https://blog.csdn.net/kevin26j/article/details/7573670)
- [C#中获取当前日期和时间的几种方法](https://www.dotblogs.com.tw/bowwowxx/2018/06/11/160146)
- [DateTime结构文档](https://docs.microsoft.com/zh-cn/dotnet/api/system.datetime)

## 参见
- [Markdown语法指南](https://www.jianshu.com/p/191d1e21f7ed)
- [C#基础教程](https://www.runoob.com/csharp/csharp-tutorial.html)