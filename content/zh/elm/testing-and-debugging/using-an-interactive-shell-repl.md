---
date: 2024-01-26 04:13:42.582452-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A Elm\u81EA\u8EAB\u5E76\u4E0D\u5185\u7F6E\
  REPL\u3002\u4F46\u662F\uFF0C\u5B89\u88C5Elm\u4E4B\u540E\uFF0C\u60A8\u53EF\u4EE5\u901A\
  \u8FC7\u547D\u4EE4\u884C\u4F7F\u7528`elm repl`\u6765\u542F\u52A8\u4E00\u4E2AElm\u4F1A\
  \u8BDD\u3002"
lastmod: '2024-03-13T22:44:47.674437-06:00'
model: gpt-4-0125-preview
summary: "Elm\u81EA\u8EAB\u5E76\u4E0D\u5185\u7F6EREPL\u3002\u4F46\u662F\uFF0C\u5B89\
  \u88C5Elm\u4E4B\u540E\uFF0C\u60A8\u53EF\u4EE5\u901A\u8FC7\u547D\u4EE4\u884C\u4F7F\
  \u7528`elm repl`\u6765\u542F\u52A8\u4E00\u4E2AElm\u4F1A\u8BDD."
title: "\u5728\u7F16\u7A0B\u4E2D\u4F7F\u7528\u4EA4\u4E92\u5F0FShell\uFF08REPL\uFF09"
weight: 34
---

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
