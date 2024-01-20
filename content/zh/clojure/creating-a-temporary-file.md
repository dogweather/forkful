---
title:                "创建临时文件"
html_title:           "Kotlin: 创建临时文件"
simple_title:         "创建临时文件"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/clojure/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# 什么与为什么？

创建临时文件意味着在计算机的临时存储位置生成一个具有唯一名称的文件。程序员这样做是为了存储暂时需要但之后可能不需要的数据。

# 如何操作：

在Clojure中创建临时文件，我们可以使用`java.nio.file`库。这是一个简单的例子说明如何做，

```Clojure
(import 'java.nio.file.Files
        'java.nio.file.Paths)

(defn create-temp-file []
  (.toAbsolutePath 
    (Files/createTempFile 
      (Paths/get (System/getProperty "java.io.tmpdir") (into-array String [])) 
      "temp" 
      ".txt")))
```
运行`create-temp-file`函数，它会在你的临时文件夹里创建一个新的临时文本文件，并返回这个文件的绝对路径：
```Clojure
(create-temp-file)
;=> #object[sun.nio.fs.UnixPath 0x1a3b2b78 "/tmp/temp114142592585910541.txt"]
```
# 深度探讨

历史背景：从Java 7开始，Java提供了`java.nio.file`库来处理文件和文件系统。Clojure作为JVM语言，可以完全访问和利用这个库。

替代方案：除了使用`java.nio.file`库，我们还可以使用Java的`java.io.File`类创建临时文件。不过，“nio”库更现代，对异步和非阻塞I/O提供了更好的支持。

实现细节：`createTempFile`方法从系统属性`java.io.tmpdir`获取临时目录的路径，然后在这个路径上创建一个唯一的新文件。其生成的临时文件名由随机值构成，以“temp”为前缀，以“.txt”为后缀。

# 另请参阅

有关`java.nio.file`库和Clojure文件I/O的更多信息：

1. [`java.nio.file`文档](https://docs.oracle.com/javase/8/docs/api/java/nio/file/package-summary.html)