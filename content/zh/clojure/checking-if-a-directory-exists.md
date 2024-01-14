---
title:                "Clojure: 检查目录是否存在"
simple_title:         "检查目录是否存在"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/clojure/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# 为什么要检查目录是否存在？

在编写Clojure程序时，需要经常检查某个目录是否存在。这样可以确保程序运行时不会出现错误，同时也可以避免不必要的麻烦。下面我们将介绍如何在Clojure中检查目录是否存在，以及更深入的信息。

## 如何进行检查

使用Clojure中的`clojure.java.io/file`函数可以方便地检查目录是否存在。以下是一个例子：

```Clojure
(require '[clojure.java.io :as io])

(let [dir (io/file "path/to/directory")]
  (if (.exists dir) ; 使用.exists方法检查目录是否存在
    (println "目录存在")
    (println "目录不存在")))
```
执行上述代码后，如果目录存在，则会输出"目录存在"，否则会输出"目录不存在"。

## 深入了解

除了使用`.exists`方法外，我们也可以使用`.isDirectory`方法来检查某个目录是否为一个有效的目录。如果文件路径指向一个目录，则返回`true`，否则返回`false`。示例如下：

```Clojure
(require '[clojure.java.io :as io])

(let [dir (io/file "path/to/directory")]
  (if (.isDirectory dir)
    (println "该目录有效")
    (println "该路径并非目录")))
```
如果指定的路径确实是一个有效的目录，则会输出"该目录有效"，否则会输出"该路径并非目录"。

## 参考链接

- 官方Clojure文档：https://clojure.org/api/java.io
- 有关常用函数和方法的详细信息：https://clojure-doc.org/cheatsheet/functions.html#file-and-directory-functions
- 关于文件和目录操作的更多细节：https://github.com/clojure-cookbook/clojure-cookbook/tree/master/02_local-development/2-13_file-io

# 查看也可

- 官方Java文档：https://docs.oracle.com/javase/tutorial/essential/io/fileOps.html
- 其他编程语言中检查目录是否存在的方法：https://stackoverflow.com/questions/4015477/how-to-test-if-a-directory-exists-in-a-shell-script/4015695#4015695
- 关于文件和目录操作的更多细节：https://en.wikipedia.org/wiki/File_system#Directories