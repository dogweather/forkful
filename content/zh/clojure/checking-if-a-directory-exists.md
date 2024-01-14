---
title:                "Clojure: 检查目录是否存在"
programming_language: "Clojure"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/clojure/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 为什么

在编写任何程序时，我们都会经常需要检查某个目录是否存在，这可以帮助我们保证程序的稳定性和顺利运行。在Clojure编程中，检查一个目录是否存在同样是必须掌握的基本技能。接下来，我们将介绍如何在Clojure中检查目录是否存在。

## 如何做

首先，让我们来创建一个目录，用于测试我们的代码。在Clojure中，我们可以使用```java.io.File/mkdirs```函数来创建目录，如下所示：

```
;; 创建一个test目录
(java.io.File/mkdirs (java.io.File. "test"))
```

接下来，我们可以使用```java.io.File/exists?```函数来检查目录是否存在，如下所示：

```
;; 检查test目录是否存在
(java.io.File/exists? (java.io.File. "test"))
```

如果目录存在，则会返回```true```，反之则返回```false```。为了更加直观地展示代码的运行结果，我们可以使用```println```函数来输出结果，如下所示：

```
;; 检查test目录是否存在，并输出结果
(println (java.io.File/exists? (java.io.File. "test")))
```

上述代码的输出结果将会是```true```，因为我们在前面已经创建了名为```test```的目录。如果我们将目录的名称更改为一个不存在的目录，则会输出```false```。

## 深入学习

在Clojure中，检查一个目录是否存在的本质是通过```java.io.File```类中的```exists?```函数来实现的。这个函数会根据文件或目录的路径返回一个布尔值，用以表示该路径下是否存在一个文件或目录。同时，```java.io.File```类中还有许多其他有用的函数，可以帮助我们进行文件和目录的操作，比如创建文件、删除文件、重命名文件等。如果对这些函数感兴趣，可以查阅Clojure官方文档来学习更多信息。

## 参考链接

- Clojure官方文档：https://clojure.org/api/cheatsheet
- ```java.io.File```类文档：https://docs.oracle.com/javase/8/docs/api/java/io/File.html

## 参见

- CLojure官方文档：https://clojure.org/
- Clojure社区论坛：https://clojureverse.org/
- Clojure中文论坛：http://clojure.cn/