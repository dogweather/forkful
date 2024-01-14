---
title:                "Java: 写作标准错误。"
simple_title:         "写作标准错误。"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/java/writing-to-standard-error.md"
---

{{< edit_this_page >}}

#为什么要写标准错误？

写作standard error对于Java程序员来说是一个非常常见的任务。它是一种日志记录方式，可以帮助开发人员追踪和调试代码中的错误。通过在代码中写入错误信息，可以更轻松地找到问题的根源并解决它们。

##如何做

为了写入standard error，需要使用Java的System类中的方法。下面是一个简单的代码示例，展示如何将错误信息写入standard error，并输出到控制台：

```Java
System.err.println("这是一条错误信息");
```

输出结果应该是：

```
这是一条错误信息
```

##深入探讨

笔记：“err”在System类中是一个输出流，它专门用于写入错误信息。与标准输出（System.out）相比，标准错误（System.err）具有不同的颜色，可以让开发人员更容易地区分它们。

此外，开发人员也可以使用printStackTrace()方法来打印完整的错误堆栈信息，这样可以更方便地定位问题。

##参考资料

* [Java System类API文档](https://docs.oracle.com/javase/8/docs/api/java/lang/System.html)
* [标准错误和标准输出的区别](https://blog.csdn.net/w2314335/article/details/44816323)
* [Java异常处理](https://www.runoob.com/java/java-exceptions.html)

#参考链接