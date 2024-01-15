---
title:                "将字符串转换为小写"
html_title:           "Haskell: 将字符串转换为小写"
simple_title:         "将字符串转换为小写"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/haskell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 为什么

将字符串转换为小写形式可能是数据处理过程中的一个常见任务。例如，当需要将用户输入的用户名标准化为小写形式时，或者在进行字符串比较时忽略大小写，都会用到这个功能。

## 如何做

下面是一个示例代码，在Haskell中如何将字符串转换为小写形式：

```
Haskell
toLower :: String -> String
toLower = map toLower
```

输入："HELLO WORLD"

输出："hello world"

## 深入探讨

在Haskell中，我们可以使用`map`函数将一个函数应用到一个列表中的每个元素上。在上面的例子中，我们将`toLower`函数应用到字符串的每个字符上，从而实现了字符串转换为小写的功能。另外值得注意的是，Haskell中的字符串实际上是由字符列表构成的，所以我们可以直接使用`map`函数，而不需要额外的处理。

## 参考链接

- [Haskell函数式编程入门](https://www.runoob.com/haskell/haskell-tutorial.html)
- [Haskell官方文档](https://www.haskell.org/documentation/)