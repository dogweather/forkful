---
title:    "Clojure: 将字符串的首字母大写"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## 为什么

在编程的世界里，一个常见的任务就是要将字符串的首字母大写。这个简单的操作可以使得字符串的外观更加美观，也能帮助用户更快地识别文字。在Clojure中，我们有一些简单的方法来实现这个任务，下面就让我们来看看吧！

## 如何做

```Clojure
;; 定义一个字符串
(def str "clojure programming")

;; 使用capitalize函数来将首字母大写
(def capital-str (capitalize str))

;; 输出结果
(print capital-str)

;; 输出："Clojure programming"
```

我们也可以使用`capitalize-words`函数来将字符串中每个单词的首字母都大写：

```Clojure
;; 定义一个字符串
(def str "this is a sample sentence")

;; 使用capitalize-words函数来将每个单词的首字母大写
(def capital-str (capitalize-words str))

;; 输出结果
(print capital-str)

;; 输出："This Is A Sample Sentence"
```

## 深入了解

在Clojure中，实现字符串首字母大写的方法有很多种，我们也可以自己定义一个函数来实现这个功能。下面是一个示例，使用`replace-first`函数来替换第一个字符为大写字母：

```Clojure
(defn capitalize-str [str]
 (str/replace-first str (subs str 0 1) (str/capitalize (subs str 0 1))))

;; 调用自定义函数
(def capital-str (capitalize-str "clojure programming"))

;; 输出结果
(print capital-str)

;; 输出："Clojure programming"
```

## 参考链接

- [Clojure字符串函数](https://clojuredocs.org/clojure.string)
- [Clojure字符串处理教程](https://www.tutorialspoint.com/clojure/clojure_string_manipulations.htm)
- [Clojure String文档](https://clojure.github.io/clojure/clojure.string-api.html#clojure.string/replace-first)