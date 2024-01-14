---
title:    "Elm: 将字符串转换为小写"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/elm/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 为什么要将字符串转换为小写

字符串转换为小写是一个常见的编程需求，尤其是在处理用户输入时。通过将字符串转换为统一的小写格式，可以更轻松地进行比较和验证，从而减少代码中的错误。

## 如何实现

使用Elm中的`String.toLower`函数可以轻松将字符串转换为小写。下面是一个简单的例子：

```elm
import String exposing (toLower)

name = "ELM PROGRAMMING"

toLowerCase name
-- output: "elm programming"
```

通过将`name`变量传递给`toLower`函数，我们可以得到一个小写的字符串输出。这在处理用户输入时特别有用，因为它可以忽略输入时的大小写差异。

## 深入探讨

在Elm中，字符串是不可变的，这意味着一旦创建，就无法改变。因此，在使用`String.toLower`函数时，它会创建一个新的小写字符串，而不会影响原始字符串。同时，该函数遵循Unicode标准，可以正确地将非英语字符转换为小写形式。

另外，如果需要将字符串中的所有字符都转换为大写，可以使用`String.toUpper`函数。

## 参考链接

- Elm官方文档：https://guide.elm-lang.org/strings.html
- Unicode标准：https://unicode.org/standard/standard.html
- 更多字符串处理函数：https://package.elm-lang.org/packages/elm/core/latest/String