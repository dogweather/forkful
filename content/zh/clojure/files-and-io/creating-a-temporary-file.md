---
date: 2024-01-20 17:39:44.957746-07:00
description: "\u521B\u5EFA\u4E34\u65F6\u6587\u4EF6\u5141\u8BB8\u7A0B\u5E8F\u4E34\u65F6\
  \u5B58\u50A8\u6570\u636E\u3002\u7A0B\u5E8F\u5458\u901A\u5E38\u8FD9\u6837\u505A\u662F\
  \u4E3A\u4E86\u6D4B\u8BD5\u3001\u5904\u7406\u5927\u6587\u4EF6\uFF0C\u6216\u8005\u5F53\
  \u4E0D\u5E0C\u671B\u6570\u636E\u957F\u671F\u5B58\u5728\u78C1\u76D8\u4E0A\u65F6\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:47.325420-06:00'
model: gpt-4-1106-preview
summary: "\u521B\u5EFA\u4E34\u65F6\u6587\u4EF6\u5141\u8BB8\u7A0B\u5E8F\u4E34\u65F6\
  \u5B58\u50A8\u6570\u636E\u3002\u7A0B\u5E8F\u5458\u901A\u5E38\u8FD9\u6837\u505A\u662F\
  \u4E3A\u4E86\u6D4B\u8BD5\u3001\u5904\u7406\u5927\u6587\u4EF6\uFF0C\u6216\u8005\u5F53\
  \u4E0D\u5E0C\u671B\u6570\u636E\u957F\u671F\u5B58\u5728\u78C1\u76D8\u4E0A\u65F6\u3002\
  ."
title: "\u521B\u5EFA\u4E34\u65F6\u6587\u4EF6"
weight: 21
---

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
