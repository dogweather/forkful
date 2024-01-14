---
title:    "Clojure: 创建临时文件"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## 为什么

在编程过程中，有时我们需要在程序运行期间创建临时文件。这些临时文件可能用于存储中间结果、缓存数据或者临时记录一些信息。创建临时文件可以帮助我们更有效地处理数据和完成任务。

## 如何

我们可以使用Clojure语言内置的函数`java.io.File/createTempFile`来创建临时文件。这个函数接受两个参数：前缀和后缀，其中后缀可以为空。让我们来看个示例：

```Clojure
(def temp-file (java.io.File/createTempFile "data" ".txt"))
```

上面的代码将创建一个名为"data"开头，后缀为".txt"的临时文件。我们可以通过使用`to-uri`函数来获取临时文件的路径：

```Clojure
(clojure.java.io/file (to-uri temp-file))
```

输出结果为`"path/to/temporary/data12595924819275882884.txt"`。现在我们可以在这个临时文件中进行数据操作，完成任务后，只需调用`(.deleteOnExit temp-file)`函数即可删除临时文件。

## 深入探讨

`createTempFile`函数的实现基于Java标准库中的`java.io.File`类。它使用系统的默认临时文件目录作为文件的父目录，并在文件名中添加随机数以确保文件名的唯一性。注意，这个函数返回的是一个Java对象，而不是Clojure数据类型。因此，我们需要使用`to-uri`函数来将其转换成Clojure中的字符串类型。

除了上述方法，我们也可以使用`with-open`来创建临时文件。这个函数可以自动关闭文件句柄，并在程序退出时自动删除临时文件。下面是一个例子：

```Clojure
(with-open [temp-file (File/createTempFile "images" nil)]
  ;; 在这里进行操作
  (.deleteOnExit temp-file))
```

## 参考资料

[Java官方文档 - createTempFile](https://docs.oracle.com/javase/8/docs/api/java/io/File.html#createTempFile-java.lang.String-java.lang.String-)

[Clojure官方文档 - java.io.File](https://clojure.github.io/clojure/clojure.java.io-api.html#clojure.java.io/to-uri)

## 参见

- [Clojure语言指南](https://clojure.org/guides/getting_started)
- [Java标准库文档](https://docs.oracle.com/javase/8/docs/api/)