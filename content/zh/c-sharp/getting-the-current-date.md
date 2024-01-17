---
title:                "获取当前日期"
html_title:           "C#: 获取当前日期"
simple_title:         "获取当前日期"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c-sharp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 什么 & 为什么?

"获取当前日期"指的是从计算机中获取当前日期和时间的功能。程序员需要这个功能来处理日期和时间相关的任务，比如记录日志、定时任务等等。

## 怎么做?

### 使用DateTime类

在C#中，我们可以很方便地使用DateTime类来获取当前日期和时间。以下是一个简单的示例代码：

```C#
DateTime now = DateTime.Now;
```

以上代码将会从计算机中获取当前的日期和时间，并赋值给一个DateTime类型的变量`now`。

### 格式化日期

如果你需要按照特定的格式来显示当前日期和时间，可以使用`ToString()`方法和格式化字符串来实现。比如，以下代码将以"月/日/年 时:分:秒"的格式来显示当前时间：

```C#
DateTime now = DateTime.Now;
string formattedDate = now.ToString("M/d/yyyy H:mm:ss");
Console.WriteLine(formattedDate);
//输出结果： 03/24/2021 14:55:00
```

### 获取特定的日期部分

如果你只需要获取日期的特定部分，比如年、月、日等，可以使用`DateTime`类的不同属性来实现。以下是一些示例代码：

```C#
DateTime now = DateTime.Now;
int year = now.Year; //获取年份
int month = now.Month; //获取月份
int day = now.Day; //获取日
int hour = now.Hour; //获取小时
int minute = now.Minute; //获取分钟
int second = now.Second; //获取秒钟
```

### 获取当前时区

如果你需要获取当前使用的时区信息，可以使用`TimeZoneInfo`类来实现。以下是一个示例代码：

```C#
TimeZoneInfo localZone = TimeZoneInfo.Local;
Console.WriteLine(localZone.DisplayName);
//输出结果：China Standard Time
```

## 深入了解

### 历史背景

在早期的计算机系统中，并没有内置的日期和时间功能，因此程序员需要手动编写代码来获取当前日期和时间。随着操作系统和编程语言的发展，现在的程序员通过内置的日期和时间类，可以更方便地获取当前日期和时间。

### 其他方法

除了使用`DateTime`类外，还有一些其他方法可以获取当前日期和时间。比如，你可以使用Windows API函数来获取当前日期和时间，或者直接从网络上的时间服务器获取准确的日期和时间。

### 实现细节

在C#中，获取当前日期和时间是通过调用操作系统提供的系统调用来实现的。具体的实现细节会依赖于操作系统和语言环境。

## 参考资料

- [DateTime.Now Property (System)  | Microsoft Docs](https://docs.microsoft.com/en-us/dotnet/api/system.datetime.now)
- [C# 获取当前日期和时间 | 菜鸟教程](https://www.runoob.com/csharp/csharp-datetime.html)
- [DateTime.Now not inserting the current date | Stack Overflow](https://stackoverflow.com/questions/10596388/datetime-now-not-inserting-the-current-date)