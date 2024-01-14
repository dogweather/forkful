---
title:                "Elm: 求取字符串的长度"
simple_title:         "求取字符串的长度"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elm/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

##为什么

总是有时候，我们需要知道一个字符串有多少个字符。无论是为了校验用户输入的长度，或者是为了其他的功能，找到字符串的长度都是一个必要的步骤。在这篇文章中，我们将学习如何在 Elm 中找到字符串的长度，让我们开始吧！

##如何做

首先，让我们创建一个包含字符串的变量。在 Elm 中，这可以通过使用`String`库中的`fromInt`函数来实现：

```Elm
import String exposing (fromInt)

myString = "欢迎来到 Elm 周日"
```

接下来，我们使用`String.length`函数来获取字符串的长度。注意，这个函数只能在包含字符串的变量后面使用：

```Elm
import String exposing (fromInt, length)

myString = "欢迎来到 Elm 周日"
myStringLength = String.length myString

-- 输出：10
```

如你所见，`myStringLength`现在的值是字符串`myString`的长度，我们可以继续使用这个变量做一些其他的操作。

##深入探究

现在让我们看一下`String.length`函数背后的逻辑。实际上，这个函数可以被定义为一个简单的递归，如下所示：

```Elm
length : String -> Int
length str =
    case str of
        "" ->
            0

        _ ->
            1 + length (dropLeft 1 str)
```

这段代码中，我们使用了`case`表达式，它会根据传入的字符串来执行不同的逻辑。如果传入的字符串是空的，那么函数会返回0作为字符串的长度。否则，我们会使用`dropLeft`函数来剔除字符串的第一个字符，并将剩余的字符串传递给`length`函数，然后再加上1。这就是我们能够获得字符串长度的逻辑。

##请参阅

- [Elm官方文档中有关String库的更多信息](https://package.elm-lang.org/packages/elm/core/latest/String)
- [了解更多关于Elm语言的知识](https://guide.elm-lang.org/)