---
title:                "字符串大写化"
aliases:
- /zh/clojure/capitalizing-a-string.md
date:                  2024-02-03T19:04:56.244219-07:00
model:                 gpt-4-0125-preview
simple_title:         "字符串大写化"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/clojure/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 什么 & 为什么？
字符串首字母大写涉及修改字符串，使其第一个字符为大写，而字符串的其余部分保持不变。程序员经常执行字符串首字母大写操作，以确保数据的一致性，特别是对于名称和地点，或者符合用户界面的语法规则。

## 如何操作：
Clojure作为一种JVM语言，允许你直接使用Java String方法。这里有一个如何在Clojure中将字符串首字母大写的基本示例：

```clojure
(defn capitalize-string [s]
  (if (empty? s)
    s
    (str (clojure.string/upper-case (subs s 0 1)) (subs s 1))))

(capitalize-string "hello world!") ; => "Hello world!"
```

Clojure没有包含一个专门用于字符串首字母大写的内建函数，但如所示，你可以通过组合`clojure.string/upper-case`、`subs`和`str`函数轻松实现这一点。

对于更简洁的解决方案和处理更复杂的字符串操作，你可能会转向第三方库。在Clojure生态系统中，一个此类受欢迎的库是`clojure.string`。然而，根据我最后的更新，它没有提供一个直接的`capitalize`函数超出核心Clojure功能所演示的，因此上面显示的方法是你不引入专门用于首字母大写的额外库的直接方法。

记住，当在Clojure中处理与Java方法交互的字符串时，你实际上是在处理Java字符串，如果必要，使你能够在你的Clojure代码中直接利用Java的String方法的全部武器库。
