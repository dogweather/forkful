---
title:                "写入标准错误"
aliases:
- /zh/elm/writing-to-standard-error.md
date:                  2024-02-03T19:33:15.512845-07:00
model:                 gpt-4-0125-preview
simple_title:         "写入标准错误"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elm/writing-to-standard-error.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 什么以及为什么？

将错误信息和诊断输出到标准错误（stderr）是指将这些信息从主程序输出（标准输出，stdout）中分离出来的一种做法。程序员这样做是为了使错误处理和日志记录更加易于管理，尤其是在输出区分对于调试和监控至关重要的环境中。

## 如何操作：

Elm 主要针对的是 web 开发，在这个领域中，直接写入 stderr 的概念并不像在传统命令行环境中那样适用。然而，对于在 Node.js 或类似环境中运行的 Elm 程序来说，使用端口与 JavaScript 互操作是实现类似功能的关键方法。以下是你可能会设置的方式：

Elm 代码（`Main.elm`）:
```elm
port module Main exposing (main)

import Browser

port errorOut : String -> Cmd msg

-- 向 JS 发送错误信息的示例函数
generateError : String -> Cmd msg
generateError message =
    errorOut message

main =
    generateError "This is an error message for stderr"
```

JavaScript 互操作（`index.js`）:
```javascript
const { Elm } = require('./Main.elm');

var app = Elm.Main.init();

app.ports.errorOut.subscribe((message) => {
  console.error(message);
});
```

这段 Elm 代码定义了一个端口 `errorOut`，允许将消息从 Elm 发送到 JavaScript。然后在 JavaScript 代码中，我们监听通过此端口发送的消息，并使用 `console.error()` 将它们重定向到 stderr。通过这种方式，你可以有效地在支持的环境中写入 stderr，利用 Elm 与 JavaScript 的互操作功能。

在 Node.js 终端中运行 `index.js` 时的示例输出：
```
This is an error message for stderr
```
