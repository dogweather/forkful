---
title:    "Clojure: 将日期转换为字符串"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/clojure/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# 为什么

在日常的软件开发中，经常会遇到需要将日期转换为字符串的情况。这是因为我们通常要在用户界面上显示日期，而用户界面只能接受字符串类型的数据。因此，对日期进行字符串转换是必不可少的步骤。在本篇文章中，我将向大家介绍如何在Clojure中进行日期和字符串之间的转换。

## 如何做

要在Clojure中将日期转换为字符串，我们可以使用内置的`format`函数。这个函数可以接受两个参数：日期对象和格式字符串。下面是一个简单的例子：

```Clojure
(format (java.util.Date.) "dd.MM.yyyy")
```

输出为：

```Clojure
"16.07.2021"
```

在这个例子中，我们使用了当前时间来作为日期对象，然后使用`dd.MM.yyyy`格式来将其转换为字符串。这个格式字符串的含义是：`dd`代表日期，`MM`代表月份，`yyyy`代表年份。你可以根据自己的需要定义不同的格式，比如`dd/MM/yyyy`或`MM-dd-yyyy`等等。

除了日期对象外，我们也可以使用`java.time`库中的`LocalDateTime`来创建一个日期对象，并进行字符串转换。下面是一个示例代码：

```Clojure
(require '[java.time :as time])

(format (time/LocalDateTime.now) "MM/dd/yyyy hh:mm a")
```

输出为：

```Clojure
"07/16/2021 05:30 PM"
```

在这个例子中，我们使用了`LocalDateTime`的`now`方法来获取当前时间，并使用`MM/dd/yyyy hh:mm a`格式来将其转换为字符串。其中，`a`代表上午/下午标记。

## 深入探讨

在进行日期和字符串转换时，我们需要注意一些细节。首先，日期格式字符串中的字母只能是`d`、`M`、`y`、`h`、`m`和`a`。其他的字母和符号会被原样输出，因此请确保格式正确。另外，你也可以在格式字符串中添加分隔符，比如`dd/MM/yyyy`或`MM-dd-yyyy`。

其次，我们还可以使用`~`符号来对转换后的结果进行格式化。比如，`~20T`可以在转换为字符串后添加空格来补齐长度，`~$`可以在转换为字符串后添加货币符号。你可以通过查阅Clojure的文档来了解更多`~`符号的用法。

最后，除了使用`format`函数，我们也可以使用`str`函数来将日期对象转换为字符串。这个函数会直接使用日期对象的`toString`方法，并输出一个带有时间戳的字符串。因此，使用`str`函数并不推荐，除非你只关心日期的时间戳信息。

# 参考资料

1. [Clojure官方文档 - 日期格式化](https://clojure.org/reference/reader#_ratio_and_datetime_literals)
2. [Java官方文档 - 日期格式化](https://docs.oracle.com/javase/8/docs/api/java/text/SimpleDateFormat.html)
3. [Clojure官方文档 - format函数](https://clojuredocs.org/clojure.core/format)
4. [Clojure官方文档 - str函数](https://clojuredocs.org/clojure.core/str)
5. [Java官方文档 - LocalDateTime类](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDateTime.html) 

---
# 参见

- [Clojure官方文档](https://clojuredocs.org/)
- [Clojure中国社区](https://clojure.org.cn/)
- [Clojure怎样转换日期？](https://www.zhihu.com/question/37150152)