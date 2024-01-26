---
title:                "在编程中使用交互式Shell（REPL）"
date:                  2024-01-26T04:13:42.582452-07:00
model:                 gpt-4-0125-preview
simple_title:         "在编程中使用交互式Shell（REPL）"
programming_language: "Elm"
category:             "Elm"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elm/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## 什么和为什么？
读取-求值-打印循环（REPL）是一个简单的交互式编程环境，它接收用户的单一输入，对其进行求值，并将结果返回给用户。Elm程序员使用REPL进行快速实验、调试或学习该语言。

## 如何操作：
Elm自身并不内置REPL。但是，安装Elm之后，您可以通过命令行使用`elm repl`来启动一个Elm会话。

```Elm
> import List exposing (..)
> map (\x -> x * 2) [1, 2, 3, 4]
[2,4,6,8] : List number
```

在这个会话中，导入List函数后，我们将列表中的数字翻倍，立即得到了结果。

## 深入了解
与Python或JavaScript等其他一些语言的REPL相比，Elm的REPL可能看起来有限制，因为Elm是一种编译型语言，专注于生成Web应用程序。从历史上看，Elm专注于完整的应用程序，而不是脚本编写或shell交互。

Elm REPL的替代品包括`elm-live`和像Ellie这样的在线编辑器，在这些编辑器中，您可以实时在浏览器中看到代码的变化。

就实现而言，Elm REPL在后台将Elm代码片段编译成JavaScript，允许您交互式地运行Elm。这与解释型语言的REPL不同，后者不需要这个编译步骤。Elm REPL也被简化，以保持核心语言轻量级和专注。

## 另请参见
- Elm关于互动性的官方指南：https://guide.elm-lang.org/interop/
- Ellie, 一个在线Elm游乐场：https://ellie-app.com/new
- `elm-live`，一个灵活的Elm开发服务器：https://www.elm-live.com/