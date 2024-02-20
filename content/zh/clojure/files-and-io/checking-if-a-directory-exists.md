---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:07:07.815037-07:00
description: "\u5728Clojure\u4E2D\u68C0\u67E5\u76EE\u5F55\u662F\u5426\u5B58\u5728\u6D89\
  \u53CA\u4ECE\u60A8\u7684Clojure\u5E94\u7528\u7A0B\u5E8F\u5185\u90E8\u9A8C\u8BC1\u6587\
  \u4EF6\u7CFB\u7EDF\u76EE\u5F55\u7684\u5B58\u5728\u3002\u8FD9\u9879\u4EFB\u52A1\u5BF9\
  \u4E8E\u6587\u4EF6\u64CD\u4F5C\u81F3\u5173\u91CD\u8981\uFF0C\u4EE5\u9632\u6B62\u5728\
  \u8BFB\u53D6\u6216\u5199\u5165\u53EF\u80FD\u4E0D\u5B58\u5728\u7684\u76EE\u5F55\u65F6\
  \u51FA\u9519\uFF0C\u786E\u4FDD\u4EE3\u7801\u6267\u884C\u7684\u5065\u58EE\u6027\u548C\
  \u65E0\u8BEF\u3002"
lastmod: 2024-02-19 22:05:06.402897
model: gpt-4-0125-preview
summary: "\u5728Clojure\u4E2D\u68C0\u67E5\u76EE\u5F55\u662F\u5426\u5B58\u5728\u6D89\
  \u53CA\u4ECE\u60A8\u7684Clojure\u5E94\u7528\u7A0B\u5E8F\u5185\u90E8\u9A8C\u8BC1\u6587\
  \u4EF6\u7CFB\u7EDF\u76EE\u5F55\u7684\u5B58\u5728\u3002\u8FD9\u9879\u4EFB\u52A1\u5BF9\
  \u4E8E\u6587\u4EF6\u64CD\u4F5C\u81F3\u5173\u91CD\u8981\uFF0C\u4EE5\u9632\u6B62\u5728\
  \u8BFB\u53D6\u6216\u5199\u5165\u53EF\u80FD\u4E0D\u5B58\u5728\u7684\u76EE\u5F55\u65F6\
  \u51FA\u9519\uFF0C\u786E\u4FDD\u4EE3\u7801\u6267\u884C\u7684\u5065\u58EE\u6027\u548C\
  \u65E0\u8BEF\u3002"
title: "\u68C0\u67E5\u76EE\u5F55\u662F\u5426\u5B58\u5728"
---

{{< edit_this_page >}}

## 什么与为什么？
在Clojure中检查目录是否存在涉及从您的Clojure应用程序内部验证文件系统目录的存在。这项任务对于文件操作至关重要，以防止在读取或写入可能不存在的目录时出错，确保代码执行的健壮性和无误。

## 如何操作：
Clojure作为一种JVM语言，可以为此目的使用Java的`java.io.File`类。您不需要任何第三方库来完成这样一个基本操作。以下是您可以如何做到的：

```clojure
(import 'java.io.File)

(defn directory-exists? [dir-path]
  (let [dir (File. dir-path)]
    (.exists dir)))

;; 使用示例
(println (directory-exists? "/path/to/your/directory")) ;; true 或 false
```

这个函数，`directory-exists?`，接受一个目录路径作为字符串，并且如果目录存在则返回`true`，否则返回`false`。这是通过创建一个带有目录路径的`File`对象，然后在这个对象上调用`.exists`方法来实现的。

除了原始的Java互操作之外，您还可以使用抽象掉一些Java样板代码的Clojure库。其中一个库是`clojure.java.io`。然而，对于检查目录是否存在，您仍然会使用`File`类，但您可能会发现该库对其他文件操作很有用。示例：

```clojure
(require '[clojure.java.io :as io])

(defn directory-exists?-clojure [dir-path]
  (.exists (io/file dir-path)))

;; 示例用法
(println (directory-exists?-clojure "/another/path/to/check")) ;; true 或 false
```

这个版本非常相似，但使用Clojure的`io/file`函数来创建`File`对象。这种方法通过利用Clojure的IO操作库，而不是直接与Java类接口，更自然地融入Clojure代码库。
