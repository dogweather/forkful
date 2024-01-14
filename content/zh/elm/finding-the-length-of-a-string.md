---
title:                "Elm: 提取字符串的长度"
programming_language: "Elm"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elm/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# 为什么
有时候在编程过程中，我们需要知道一个文本字符串的长度，这可以帮助我们在处理数据时更加精确和有效。在Elm中，您可以很容易地找到字符串的长度，并在所需的情况下使用它。

# 如何进行
```Elm
-- 定义一个文本字符串
myString = "Hello World"

-- 使用String.length函数来找到字符串的长度
lengthOfMyString = String.length myString

-- 打印输出
Debug.log "长度是" lengthOfMyString
```

这段代码输出的结果应该是`11`，因为字符串中共有11个字符。

# 深入
在Elm中，字符串是一种特殊类型的数据，它由字符组成，每个字符都有一个相应的码点（code point）来表示它的值。在计算字符串的长度时，Elm会自动考虑每个字符的码点，因此它可以正确地返回字符串的实际长度。

如果您想要更加深入了解Elm中字符串的处理，您可以学习如何使用码点（code points）来操作字符串，并尝试使用更多的字符串函数来处理不同类型的字符串数据。

# 参考链接
- Elm语言官方文档：https://guide.elm-lang.org
- Elm字符串函数列表：http://package.elm-lang.org/packages/elm-lang/core/latest/String

# 参见
- [`String.length`文档](http://package.elm-lang.org/packages/elm-lang/core/latest/String#length)
- 字符串处理技巧：https://medium.com/@theobot/elm-tricks-string-dealings-533d8233757