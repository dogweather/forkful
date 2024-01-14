---
title:                "Clojure: 寻找字符串的长度"
programming_language: "Clojure"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/clojure/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# 为什么要找出字符串的长度？

在编程中，经常需要操作字符串。为了正确地处理字符串，我们需要知道它们的长度。找出字符串的长度也可以帮助我们在处理文本数据时更加高效。

## 如何找出字符串的长度

我们可以使用Clojure中内置的函数`count`来计算字符串的长度。这个函数接受一个字符串作为参数，并返回该字符串的长度。让我们看一个例子：

```Clojure
(count "Hello World") ; 输出为11
(count "你好世界") ; 输出为4，因为一个汉字占用3个字符的长度
```

如上所示，我们可以通过`count`函数轻松地找出字符串的长度。它也可以处理中文字符串，因为Clojure会自动将字符串转换为UTF-8编码。

## 深入了解

在深入研究字符串长度的原理之前，我们需要了解一些概念。在计算机语言中，一个字符通常由一个字节（byte）来表示。而在Unicode编码中，一个字符可能由多个字节来表示。这就是为什么一个汉字在计算长度时会占用多个字符的原因。

Clojure中的字符串实际上是由Unicode字符序列组成的。当我们使用`count`函数计算字符串长度时，它实际上是将字符串转换为一个Unicode字符数组，然后取得数组的长度。

## 参考链接

- [Clojure官方文档 - 字符串操作](https://clojure.org/reference/strings)
- [Unicode官方网站](https://home.unicode.org/)
- [关于字符和字节的解释](https://www.ibm.com/support/knowledgecenter/zh/SSLTBW_2.2.0/com.ibm.zos.v2r2.ieab000/iecproce_charvsbyte.htm)

# 参考资料

[See Also (参考资料)]