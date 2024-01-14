---
title:                "Clojure: 创建临时文件"
programming_language: "Clojure"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/clojure/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## 为什么

临时文件是一个有用的工具，可以在程序运行时创建和使用。它可以帮助我们存储临时数据，比如说在处理大型文件时，我们可以将数据暂时存储在临时文件中，以免占用过多的内存。临时文件也可以用于测试和调试，方便我们追踪程序运行过程中的变化。

## 如何使用

使用Clojure来创建临时文件非常简单。我们可以使用Clojure内置的`with-open`函数，它能够自动关闭我们创建的临时文件。

```Clojure
(with-open [temp-file (java.io.File/createTempFile "prefix" ".txt")]
  ;; 这里可以对临时文件做一些操作，比如写入数据
  (clojure.string/write "Hello World!" temp-file))

;; 当`with-open`代码块执行完毕后，临时文件会自动被关闭和删除
```

我们也可以使用Java的`File`类来创建临时文件。

```Clojure
(let [temp-file (java.io.File/createTempFile "prefix" ".txt")]
  ;; 这里可以对临时文件做一些操作
  (clojure.string/write "Hello World!" temp-file)
  ;; 如果不手动删除临时文件，那么它会在程序退出时被自动删除
  (.deleteOnExit temp-file))
```

## 深入探讨

在Clojure中，我们可以使用`clojure.java.io/file`函数来创建`java.io.File`对象来表示一个文件。然后使用File类的`createTempFile`方法来创建临时文件。该方法接受两个参数，第一个是临时文件的前缀，第二个是临时文件的后缀。临时文件的名称会根据系统默认的命名规则生成。

临时文件默认是存在于操作系统的默认临时文件目录中的，可以通过调用`java.io.File/setDefaultTempFile`来修改默认的临时文件目录。

## 参考资料

- [Clojure函数库文档](https://clojure.github.io/clojure/clojure.java.io-api.html#clojure.java.io/file)
- [Java 8 API 概览：File类](https://docs.oracle.com/javase/8/docs/api/java/io/File.html)
- [Clojure编程视频教程](https://www.youtube.com/watch?v=qYmvzxlN438)

## 参见

[Java 临时文件和目录](https://docs.oracle.com/javase/8/docs/api/java/io/File.html#createTempFile-java.lang.String-java.lang.String-)