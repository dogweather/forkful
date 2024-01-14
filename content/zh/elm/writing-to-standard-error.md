---
title:    "Elm: 写入标准错误"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/elm/writing-to-standard-error.md"
---

{{< edit_this_page >}}

Why (为什么):

在编写Elm程序时，有时候我们会遇到错误和异常情况。这时，我们可以通过将信息写入标准错误来有效地调试和处理问题。这样可以让我们更快地找到问题并解决它，提高编程效率。

How To (如何实现):

使用Elm的```Debug```模块可以让我们方便地向标准错误写入信息。首先，在我们的程序中导入```Debug````模块。接着，我们可以使用```Debug.log```函数来输出任意类型的信息到标准错误：

```Elm
import Debug

name = "John"
Debug.log "Hi there!" name
```

上面的例子中，我们将变量```name```的值写入标准错误，输出结果如下：

```
Hi there!
"John"
```

除了简单的输出值，我们也可以通过嵌套的方式输出更复杂的信息。例如：

```Elm
import Debug

users = [ "John", "Lisa", "Sam" ]
Debug.log "Users:" users
```

输出结果为：

```
Users:
[ "John", "Lisa", "Sam" ]
```

有时候，我们想要将信息输出到标准错误但又不想影响程序的执行。这时，我们可以使用```Debug.toString```函数将信息转换为字符串，然后再使用```Debug.log```输出。例如：

```Elm
import Debug

number = 123
Debug.log "Number:" (Debug.toString number)
```

输出结果为：

```
Number:
"123"
```

Deep Dive (深入探讨):

除了使用```Debug.log```函数，我们还可以使用```Debug.crash```函数来将错误信息输出到标准错误。和```Debug.log```函数不同的是，```Debug.crash```函数会让程序在遇到错误时立即停止运行。这样可以让我们更快地发现问题所在。

```Elm
import Debug

age = -5
Debug.crash "Invalid age entered!" age
```

输出结果为：

```
-- CRASHED -----------------------------------------------

Invalid age entered!
-5
```

通过学习如何向标准错误写入信息，我们可以更加有效地调试代码，提高编程效率。在编写复杂的程序时，这是一个非常有用的技巧。

See Also (参考链接):

- [Elm官方文档-Debug模块](https://elm-lang.org/docs/debug)
- [在Elm程序里debugging](https://guide.elm-lang.org/debugging/)
- [学习Elm的调试技巧](https://medium.com/@dmyates/debugging-tips-for-learning-elm-8efb8f8d230c)