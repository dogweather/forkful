---
title:                "创建临时文件"
date:                  2024-01-20T17:39:44.957746-07:00
model:                 gpt-4-1106-preview
simple_title:         "创建临时文件"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/clojure/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？
创建临时文件允许程序临时存储数据。程序员通常这样做是为了测试、处理大文件，或者当不希望数据长期存在磁盘上时。

## 如何操作：
在Clojure中，可以使用`java.io.File`创建临时文件。下面是个简洁的示例：

```clojure
(import '(java.io File))

(defn create-temp-file [prefix suffix]
  (.createTempFile (File. (System/getProperty "java.io.tmpdir")) prefix suffix))

(let [temp-file (create-temp-file "temp" ".txt")]
  (println "Temporary file created:" (.getPath temp-file)))
```

这段代码将输出临时文件的路径，看起来像这样:

```
Temporary file created: /tmp/temp4395958501049098503.txt
```

## 深入了解
临时文件在1970年代就已经存在了，最早是在类Unix系统中使用。创建临时文件的方式多种多样，Clojure通过Java的`java.io.File`提供了一个简单且强大的方法。除了`createTempFile`方法，你还可以使用第三方库如`clojure.java.io`来处理文件。这些方法内部通常还会处理文件的删除策略，临时文件可能在JVM退出时删除，或者在程序运行完毕后删除，具体取决于方法实现。

## 参考链接
- [Clojure's `java.io` namespace documentation](https://clojure.github.io/clojure/clojure.java.io-api.html)
- [Java's File documentation](https://docs.oracle.com/javase/7/docs/api/java/io/File.html)
- [Understanding Java's Temporary Files](https://docs.oracle.com/javase/tutorial/essential/io/file.html#tempfiles)
