---
title:    "Clojure: 搜索和替换文本"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/clojure/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## 为什么

在编程中进行搜索和替换文本是非常常见的任务。它可以帮助我们快速地在大量的文本中找到特定的内容，并替换它们为我们需要的内容。无论是清理数据、完成批量操作，还是简单地修改文本，搜索和替换都是非常有用的工具。

## 如何

要在Clojure中进行搜索和替换文本，我们可以使用内置的`clojure.string`库中的`replace`函数。下面是一个简单的例子，用来替换字符串中的特定字符：

```Clojure
(require '[clojure.string :as str])

(defn replace-char [string old-char new-char]
  (str/replace string (str old-char) (str new-char)))

(replace-char "Hello World" "o" "a")
;; => "Hella Warld"
```

我们也可以使用`replace`函数来替换多个字符。例如，我们想要将字符串中所有的空格替换为下划线：

```Clojure
(require '[clojure.string :as str])

(defn replace-spaces [string]
  (str/replace string " " "_"))

(replace-spaces "This is a test")
;; => "This_is_a_test"
```

当我们想要替换的内容比较复杂时，我们可以使用正则表达式来匹配并替换。让我们尝试将字符串中所有的数字替换为星号：

```Clojure
(require '[clojure.string :as str])

(defn replace-numbers [string]
  (str/replace string #"\d" "*"))

(replace-numbers "1234abcd")
;; => "****abcd"
```

## 深入了解

除了`replace`函数，`clojure.string`中还有一些其他有用的函数可以帮助我们搜索和替换文本。例如，`split`函数可以根据指定的分隔符将字符串分割成一个列表。让我们尝试使用`split`函数来替换掉一个字符串中的所有元音字母：

```Clojure
(require '[clojure.string :as str])

(def vowels #{"a" "e" "i" "o" "u"})

(defn replace-vowels [string]
  (str/join (str/split string #"" :remove-empty? true :when (complement vowels))))

(replace-vowels "hello world")
;; => "hll wrld"
```

除了`clojure.string`库，我们也可以使用`java.lang.String`类中的`replaceAll`函数来进行搜索和替换。让我们尝试使用正则表达式来替换字符串中的所有大写字母：

```Clojure
(defn replace-uppercase [string]
  (.replaceAll string "[A-Z]" "#"))

(replace-uppercase "Hello World")
;; => "#ello #orld"
```

## 参考文献

1. [Clojure string namespace docs](https://clojuredocs.org/clojure.string)
2. [Java String class docs](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
3. [Regular Expression Tutorial](https://www.regular-expressions.info/tutorial.html) 

## 参见

- [如何在Clojure中操作字符串](https://www.example.com/article?id=123)
- [使用Clojure处理文本数据的实用技巧](https://www.example.com/article?id=456)