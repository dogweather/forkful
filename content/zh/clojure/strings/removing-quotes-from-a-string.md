---
date: 2024-01-26 03:39:40.693297-07:00
description: "\u4ECE\u5B57\u7B26\u4E32\u4E2D\u53BB\u9664\u5F15\u53F7\u610F\u5473\u7740\
  \u53BB\u6389\u90A3\u4E9B\u5305\u56F4\u4F60\u6587\u672C\u7684\u70E6\u4EBA\u7684\u53CC\
  \u5F15\u53F7\u6216\u5355\u5F15\u53F7\u5B57\u7B26\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\
  \u505A\u662F\u4E3A\u4E86\u6E05\u7406\u6570\u636E\u3001\u786E\u4FDD\u7EDF\u4E00\u6027\
  \uFF0C\u6216\u4E3A\u4E86\u51C6\u5907\u5B57\u7B26\u4E32\u8FDB\u884C\u5904\u7406\uFF0C\
  \u5728\u8FD9\u79CD\u5904\u7406\u4E2D\u5F15\u53F7\u662F\u4E0D\u9700\u8981\u7684\u6216\
  \u53EF\u80FD\u4F1A\u5BFC\u81F4\u9519\u8BEF\u3002"
lastmod: '2024-03-13T22:44:47.290161-06:00'
model: gpt-4-0125-preview
summary: "\u4ECE\u5B57\u7B26\u4E32\u4E2D\u53BB\u9664\u5F15\u53F7\u610F\u5473\u7740\
  \u53BB\u6389\u90A3\u4E9B\u5305\u56F4\u4F60\u6587\u672C\u7684\u70E6\u4EBA\u7684\u53CC\
  \u5F15\u53F7\u6216\u5355\u5F15\u53F7\u5B57\u7B26\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\
  \u505A\u662F\u4E3A\u4E86\u6E05\u7406\u6570\u636E\u3001\u786E\u4FDD\u7EDF\u4E00\u6027\
  \uFF0C\u6216\u4E3A\u4E86\u51C6\u5907\u5B57\u7B26\u4E32\u8FDB\u884C\u5904\u7406\uFF0C\
  \u5728\u8FD9\u79CD\u5904\u7406\u4E2D\u5F15\u53F7\u662F\u4E0D\u9700\u8981\u7684\u6216\
  \u53EF\u80FD\u4F1A\u5BFC\u81F4\u9519\u8BEF\u3002"
title: "\u4ECE\u5B57\u7B26\u4E32\u4E2D\u79FB\u9664\u5F15\u53F7"
---

{{< edit_this_page >}}

## 什么 & 为什么？
从字符串中去除引号意味着去掉那些包围你文本的烦人的双引号或单引号字符。程序员这样做是为了清理数据、确保统一性，或为了准备字符串进行处理，在这种处理中引号是不需要的或可能会导致错误。

## 如何操作：
在Clojure中，字符串是不可变的，所以当我们谈论“移除引号”时，我们实际上是在谈论创建一个没有引号的新字符串。使用`clojure.string/replace`的方法如下：

```clojure
(require '[clojure.string :as str])

; 让我们摆脱双引号
(defn remove-double-quotes [s]
  (str/replace s #"\"" ""))

; 并且排除单引号
(defn remove-single-quotes [s]
  (str/replace s #"\'" ""))

; 示例用法：
(remove-double-quotes "\"Hello, World!\"") ; => "Hello, World!"
(remove-single-quotes "'Hello, World!'")   ; => "Hello, World!"
```
想要一次性处理单引号和双引号？看这里：

```clojure
(defn remove-quotes [s]
  (str/replace s #"[\"\']" ""))

; 示例用法：
(remove-quotes "\"Hello, 'Clojure' World!\"") ; => "Hello, Clojure World!"
```

## 深入探索
回想起来，在数据比小孩子的卧室还乱的时代，字符串中的引号是表示文本的常态。但随着计算机科学的发展，引号不再仅仅是文本定界符——它们在编程语言中承担了语法角色。

Clojure，凭借其Lisp传承，并不像某些其他语言那样使用引号。它们确实用于表示字符串，但它们还有在创建字面值时的特殊作用。无论如何，从字符串中移除引号仍是一项永恒的任务。

为什么不只是切掉字符串的两端呢？嗯，这是基于你的引号总是像一对过分亲密的祖父母那样紧紧拥抱字符串的起始和结束。实际世界的数据更加凌乱。输入正则表达式（regex），它让你可以不管引号藏在哪里都能瞄准它们。

有替代方案吗？当然，如果你想炫耀你可以使用`subs`、`trim`、`triml`、`trimr`，甚至是转换器。但是使用正则表达式的`replace`就像是在刀战中带了一把光剑——它直接切入要点。

## 另请参阅
如果你的大脑还渴望更多Clojure字符串操作的知识，这些小贴士可能会有所帮助：

- 关于`clojure.string/replace`的ClojureDocs：https://clojuredocs.org/clojure.string/replace
- Clojure中的正则表达式：https://clojure.org/guides/learn/syntax#_regex
- 字符串处理的Java互操作（毕竟Clojure运行在JVM上）：https://clojure.org/reference/java_interop#_working_with_strings

不要仅仅停留在移除引号上。在Clojure-land里，有一个整个字符串魔法的世界等着你去发现。
