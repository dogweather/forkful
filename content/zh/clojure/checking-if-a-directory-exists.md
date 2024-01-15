---
title:                "检查目录是否存在"
html_title:           "Clojure: 检查目录是否存在"
simple_title:         "检查目录是否存在"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/clojure/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 为什么要检查目录是否存在

当我们在编写程序时，经常会涉及到读写文件的操作。有时候，我们需要检查一个目录是否存在，以便正确地处理文件路径。检查目录是否存在可以帮助我们避免程序运行中的错误，保证代码的稳定性和可靠性。

## 如何进行检查

在Clojure中，我们可以使用`(fs/exists? path)`函数来检查一个目录是否存在。这个函数接受一个路径作为参数，并返回一个布尔值来表示目录是否存在。下面是一个例子：

```Clojure
(def my-directory "C:/Users/John/Documents/") ; 定义一个目录路径
(if (fs/exists? my-directory)
  (println "目录存在")
  (println "目录不存在"))
```

如果`my-directory`目录存在，则会打印出"目录存在"；若不存在，则会打印出"目录不存在"。

## 深入了解

`fs/exists?`函数实际上是通过调用Java中的`exists()`方法来判断目录是否存在。这个方法使用Java的IO库来检查文件系统中的对象是否存在，因此可以用来检查文件、目录或者符号链接是否存在。

在Clojure中，我们还可以使用其他函数来操作文件系统，例如`(file? path)`函数来检查一个路径是否是文件，`(link? path)`函数来判断一个路径是否是符号链接。通过熟练掌握这些函数，我们可以更加灵活地操作文件系统，实现更多的功能。

## 查看更多

- [Clojure官方文档](https://clojure.org/api/cheatsheet)
- [Java IO库文档](https://docs.oracle.com/javase/7/docs/api/java/io/package-summary.html)
- [如何在Clojure中操作文件系统](https://www.braveclojure.com/files/)
- [了解Clojure中的条件语句](https://www.braveclojure.com/control-structures/)