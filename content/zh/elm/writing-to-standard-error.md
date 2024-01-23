---
title:                "写入标准错误"
html_title:           "Arduino: 写入标准错误"
simple_title:         "写入标准错误"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elm/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why? (是什么？为什么？)
写入标准错误（stderr）是输出警告或错误消息的方式，不会干扰正常输出（stdout）。这样，开发者能清晰地分辨程序输出的不同类型信息，方便调试和日志记录。

## How to: (如何操作：)
Elm 目前没有内建的写入标准错误的功能。但是，你可以用 Ports 和 JavaScript 交互来实现。下面是个例子：

```Elm
port module Main exposing (..)

-- 定义一个 Port 来发送错误信息
port stderr : String -> Cmd msg

-- 触发一个错误消息
sendError : String -> Cmd msg
sendError message =
  stderr message
```

在 JavaScript 中接收并处理：

```JavaScript
app.ports.stderr.subscribe((message) => {
  console.error(message);
});
```

现在 Elm 发送到 `stderr` 的任何消息都会在 JavaScript 控制台以错误形式显示。

## Deep Dive (深入探讨)
传统上，编程语言区分标准输出（stdout）和标准错误（stderr）来帮助开发者识别程序流程和问题。Elm 更注重前端开发，STDOUT/STDERR的概念在浏览器中并不常见。因此，Elm 中通常通过 Ports 来实现类似功能。

备选方案包括自己实现简化的日志系统，或者使用第三方服务。在某些情况下，你也可以选择简单地抛出运行时错误来代替写入stderr。

而实现这一功能的具体细节，主要涉及 Elm 和 JavaScript 之间的交互，以及浏览器的控制台 API。

## See Also (另请参阅)
- Elm 官方指南中的 Ports 说明：[官方Ports指南](https://guide.elm-lang.org/interop/ports.html)
- JavaScript `console.error` 方法: [MDN console.error](https://developer.mozilla.org/en-US/docs/Web/API/Console/error)
