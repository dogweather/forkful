---
title:                "字符串首字母大写"
date:                  2024-01-19
html_title:           "Arduino: 字符串首字母大写"
simple_title:         "字符串首字母大写"

category:             "Clojure"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/clojure/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (是什么？为什么？)
字符串首字母大写，是指让字符串中的每个单词的首字母变成大写。程序员这么做，通常是为了格式规范或用户界面的文本显示。

## How to (如何操作)
Clojure没有内建的首字母大写函数，但可以这样做：

```clojure
(defn capitalize-str [s]
  (clojure.string/join " " (map clojure.string/capitalize (clojure.string/split s #"\s+"))))

;; 使用示例:
(println (capitalize-str "ni hao, shi jie!")) ; 输出: "Ni Hao, Shi Jie!"
```

## Deep Dive (深入了解)
在历史上，CL（Common Lisp）的`string-capitalize`可能启发了当前首字母大写的实现。同样，在其他语言中也有不同的实现方式——比如Python的`.title()`。

`clojure.string/capitalize`函数只会把字符串中的第一个字符变成大写，所以我们使用`map`来逐个处理经过`split`的单词数组。在一些场景下，可能需要考虑本地化和特殊字符，这时得需要更复杂的规则。

## See Also (另请参阅)
- Clojure 字符串API文档: [clojure.string](https://clojuredocs.org/clojure.string)
- Java String文档参考，了解底层实现: [String (Java Platform SE 8)](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
- 编程规范相关讨论: [Stack Overflow](https://stackoverflow.com/questions/tagged/clojure?tab=Votes)
