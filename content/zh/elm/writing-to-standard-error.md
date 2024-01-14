---
title:    "Elm: 通过标准错误写入"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## 为什么要写到标准错误

在Elm编程中，有时候我们需要将一些错误信息打印到标准错误流（Standard Error），这可以帮助我们更好地追踪和调试程序中的问题。通过写到标准错误，我们可以更快地定位错误的位置，从而提高我们的开发效率。

## 如何写到标准错误

要将错误信息写到标准错误流，我们可以使用Elm内置的"Debug.log"函数。该函数接受两个参数，第一个是标识符（identifier），用于标识该条错误信息，第二个参数则是要打印的错误信息。

```Elm
import Debug exposing (log)
 
-- 假设我们在调用一个未定义的函数
result = myFunction 5
 
-- 我们可以在这里使用"Debug.log"函数打印错误信息
result = Debug.log "myFunction" (myFunction 5)
 
-- 控制台输出：
-- ERROR: myFunction: The function "myFunction" is not defined.
```

## 深入了解写到标准错误

除了"Debug.log"函数之外，Elm还提供了其他几个函数来帮助我们进行调试。比如"Debug.log2"用于打印两个参数，"Debug.log3"用于打印三个参数，以此类推。另外，我们也可以使用"Debug.todo"函数来标记我们还未完成的功能部分，从而在开发过程中提醒自己还需要补充完成的部分。

## 参考链接

- Elm官方文档：https://guide.elm-lang.org/
- Elm错误调试：https://thoughtbot.com/blog/testing-command-line-apps-in-elm
- Elm调试技巧：https://www.charnock.xyz/2019/07/16/elm-logging-with-debug/
 
## 参见

- [Elm官方文档（中文版）](https://guide.elm-lang-cn.org/)
- [Elm调试工具](https://package.elm-lang.org/packages/elm/core/latest/Debug)