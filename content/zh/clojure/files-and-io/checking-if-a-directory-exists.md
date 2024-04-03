---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:07:07.815037-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A Clojure\u4F5C\u4E3A\u4E00\u79CDJVM\u8BED\
  \u8A00\uFF0C\u53EF\u4EE5\u4E3A\u6B64\u76EE\u7684\u4F7F\u7528Java\u7684`java.io.File`\u7C7B\
  \u3002\u60A8\u4E0D\u9700\u8981\u4EFB\u4F55\u7B2C\u4E09\u65B9\u5E93\u6765\u5B8C\u6210\
  \u8FD9\u6837\u4E00\u4E2A\u57FA\u672C\u64CD\u4F5C\u3002\u4EE5\u4E0B\u662F\u60A8\u53EF\
  \u4EE5\u5982\u4F55\u505A\u5230\u7684\uFF1A."
lastmod: '2024-03-13T22:44:47.319155-06:00'
model: gpt-4-0125-preview
summary: "Clojure\u4F5C\u4E3A\u4E00\u79CDJVM\u8BED\u8A00\uFF0C\u53EF\u4EE5\u4E3A\u6B64\
  \u76EE\u7684\u4F7F\u7528Java\u7684`java.io.File`\u7C7B\u3002\u60A8\u4E0D\u9700\u8981\
  \u4EFB\u4F55\u7B2C\u4E09\u65B9\u5E93\u6765\u5B8C\u6210\u8FD9\u6837\u4E00\u4E2A\u57FA\
  \u672C\u64CD\u4F5C\u3002\u4EE5\u4E0B\u662F\u60A8\u53EF\u4EE5\u5982\u4F55\u505A\u5230\
  \u7684\uFF1A."
title: "\u68C0\u67E5\u76EE\u5F55\u662F\u5426\u5B58\u5728"
weight: 20
---

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
