---
title:                "Clojure: 将字符串改为大写"
programming_language: "Clojure"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/clojure/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 为什么
为什么有时候我们会需要将一个字符串中的第一个字母大写呢？这可能是因为我们想要符合某种规范，或者让字符串更易于阅读。无论原因如何，通过在Clojure中使用capitalize函数，我们可以快速简单地实现这一目的。

## 如何使用
```
Clojure (capitalize "hello")
```
输出："Hello"

```
Clojure (capitalize "apple pie")
```
输出："Apple pie"

在上述示例中，我们可以看到capitalize函数在原始字符串的第一个字母转换为大写后，返回了一个新的字符串。如果原始字符串本身就已经是大写开头的话，capitalize函数会返回原始字符串。

## 深入了解
在背后，capitalize函数实际上是调用了java.lang.String类的capitalize函数。具体来说，它将原始字符串传递给了Java类，然后使用了Java的方法来实现首字母大写的转换。

除了字符串，capitalize函数还可以接受其他类型的数据作为输入，比如数字或布尔值。在这种情况下，它会将数据先转换为字符串，然后再进行首字母大写的操作。

## 查看其他资料
- [capitalize函数的官方文档](https://clojuredocs.org/clojure.core/capitalize)
- [关于Clojure中字符串操作的更多知识](https://clojure.org/guides/learn/string)
- [其他Clojure函数的使用示例](https://clojuredocs.org)