---
title:    "Clojure: 将字符串转换为小写"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## 为什么

在编程过程中，我们经常需要处理字符串数据。而有时候，我们希望将字符串中的所有字母都转换为小写，这可以帮助我们更方便地进行比较和操作。因此，编写一个将字符串转换为小写的功能是非常有用的。

## 如何操作

假设我们有一个字符串，内容为 "HELLO WORLD"，现在我们想要将它转换为小写。在Clojure中，实现这一功能只需要简单的几行代码：

```Clojure
(defn to-lower [str]
  (clojure.string/lower-case str))

(to-lower "HELLO WORLD")
```

以上代码会输出 "hello world"，我们可以看到，字符串中的所有字母都被成功转换为小写了。同时，我们也可以将这个功能封装成一个函数，随时调用。

## 深入了解

Clojure中，转换字符串为小写的方法有多种。除了使用 `lower-case` 方法外，还可以使用 `lower-case-nfkc` 和 `lower-case-locales` 方法。这些方法都可以接受多个字符串作为参数，并返回一个新的字符串。在平时的编程中，我们可以根据具体的需求选择不同的方法来进行字符串转换。

## 参考链接

- [Clojure文档 - 字符串处理](https://clojuredocs.org/clojure.string)
- [Clojure中的字符串操作](https://gigasquidsoftware.com/blog/2018/04/01/string-manipulation-in-clojure/)
- [使用Clojure进行字符串比较和操作](https://clojureverse.org/t/string-comparison-and-operation/4795)

## 参见

- [Clojure中的字符串拼接与替换](https://github.com/zenghongtu/blog/blob/master/clojure/strings-concat-and-replace.md)
- [Clojure中的字符串分割与连接](https://github.com/zenghongtu/blog/blob/master/clojure/strings-split-and-join.md)