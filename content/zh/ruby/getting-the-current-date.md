---
title:    "Ruby: 获取当前日期"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## 为什么

为了让我们的程序能够在特定的时间点执行特定的任务，获取当前日期是非常重要的。这样，我们就可以根据日期来做出相应的判断和操作。

## 如何获取当前日期

原生的Ruby语言提供了一个非常方便的方法来获取当前日期，就是使用`Date.today`函数。让我们来看一个简单的例子：

```Ruby
current_date = Date.today
puts current_date
```
这段代码将会打印出当前的日期，比如今天是2021年7月19日，那么程序将会输出`2021-07-19`。通过这个方法，我们可以轻松地获取当天的日期，并在程序中使用。

如果你需要打印出更加易读的日期格式，可以使用`strftime`方法来指定日期的输出格式。例如，如果你想要打印出今天的日期和星期几，可以这样写：

```Ruby
current_date = Date.today
puts current_date.strftime("%Y年%m月%d日 %A")
```
这段代码将会输出`2021年07月19日 Monday`，我们可以根据自己的需求自定义日期的输出格式。

## 深入了解获取当前日期

其实，`Date.today`方法只是`Date`类中众多日期相关的方法之一。如果你想要深入了解Ruby中关于日期的更多知识，可以参考以下链接：

- [Ruby文档-Date类](https://www.ruby-doc.org/core-2.7.2/Date.html)
- [Ruby Date与Time的区别](https://www.rubyguides.com/2019/05/ruby-date-time-class/)
- [Ruby中的日期和时间操作](https://www.digitalocean.com/community/tutorials/how-to-use-dates-and-times-in-ruby)
- [Ruby中的时区操作](https://www.sitepoint.com/working-with-time-zones-in-ruby/)

## 参考链接

- [Ruby文档](https://www.ruby-lang.org/zh_cn/documentation/)
- [Ruby中文社区论坛](https://ruby-china.org/)
- [Ruby编程语言简介](https://zh.wikipedia.org/wiki/Ruby)