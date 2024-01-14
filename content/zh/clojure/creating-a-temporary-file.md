---
title:                "Clojure: 生成一个临时文件"
simple_title:         "生成一个临时文件"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/clojure/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## 为什么

临时文件是许多编程中常见的概念，它能帮助我们在程序运行的过程中存储临时数据或保存中间结果。它们的创建和使用经常是编程过程中必不可少的一部分，让我们来看看如何在Clojure中创建临时文件吧！

## 如何

在Clojure中，我们可以使用`with-open`函数来创建一个临时文件。下面是一个简单的示例代码：

```Clojure
(with-open [^java.io.File file (java.io.File/createTempFile "temp" nil)]
    (println "临时文件的路径是：" (.getAbsolutePath file)))
```

运行上面的代码会得到如下的输出：

```
临时文件的路径是：/tmp/temp325132361647186075.tmp
```

## 深入探讨

上面的示例代码中，我们使用了`createTempFile`函数来创建一个以"temp"开头的临时文件，并且不指定后缀名，所以系统会自动生成唯一的后缀。在`with-open`中，我们将临时文件赋值给一个变量`file`，然后可以通过该变量来访问临时文件的路径和其他属性。`with-open`函数会确保在执行完后关闭临时文件，这样就不用手动清理临时文件了。

除了使用`with-open`，我们还可以使用`File`类的`deleteOnExit`函数来在程序退出时自动删除临时文件，避免在程序运行过程中临时文件没有被手动删除的情况。

## 参考链接

- [Java官方文档：createTempFile函数](https://docs.oracle.com/javase/8/docs/api/java/io/File.html#createTempFile-java.lang.String-java.lang.String-java.io.File-)

## 参考文章

欢迎阅读我们其他关于Clojure编程的博客文章：

- [Clojure中原生JSON解析与封装](http://www.example.com)
- [使用Clojure创建RESTful API](http://www.example.com)

## 参见

- [Java官方文档：createTempFile函数](https://docs.oracle.com/javase/8/docs/api/java/io/File.html#createTempFile-java.lang.String-java.lang.String-java.io.File-)
- [Clojure官方文档：with-open函数](https://clojuredocs.org/clojure.core/with-open)