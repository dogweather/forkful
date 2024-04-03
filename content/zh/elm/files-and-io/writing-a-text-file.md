---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:27:58.437653-07:00
description: "\u2026"
lastmod: '2024-03-13T22:44:47.693125-06:00'
model: gpt-4-0125-preview
summary: "\u5728Elm\u4E2D\u5199\u5165\u6587\u672C\u6587\u4EF6\u6D89\u53CA\u4ECE\u4E00\
  \u4E2AElm\u5E94\u7528\u7A0B\u5E8F\u521B\u5EFA\u5E76\u4FDD\u5B58\u6587\u672C\u6570\
  \u636E\u5230\u6587\u4EF6\u3002\u7A0B\u5E8F\u5458\u7ECF\u5E38\u9700\u8981\u751F\u6210\
  \u62A5\u544A\u3001\u65E5\u5FD7\uFF0C\u6216\u4EE5\u7ED3\u6784\u5316\u6587\u672C\u683C\
  \u5F0F\uFF08\u4F8B\u5982\uFF0CJSON\u3001CSV\uFF09\u5BFC\u51FA\u6570\u636E\u4EE5\u4F9B\
  \u5176\u4ED6\u5E94\u7528\u7A0B\u5E8F\u4F7F\u7528\u6216\u8BB0\u5F55\u4FDD\u6301\u76EE\
  \u7684\u3002\u7136\u800C\uFF0C\u7531\u4E8EElm\u7684\u67B6\u6784\u5173\u6CE8\u4E8E\
  \u7EAF\u51C0\u548C\u5B89\u5168\uFF0C\u76F4\u63A5\u5199\u6587\u4EF6\u2014\u2014\u5C31\
  \u50CF\u8BB8\u591A\u5176\u4ED6\u7684\u526F\u4F5C\u7528\u4E00\u6837\u2014\u2014\u662F\
  \u901A\u8FC7\u5411\u5468\u56F4\u7684JavaScript\u73AF\u5883\u53D1\u9001\u547D\u4EE4\
  \u6765\u5904\u7406\u7684\u3002."
title: "\u7F16\u5199\u6587\u672C\u6587\u4EF6"
weight: 24
---

## 什么 & 为什么？

在Elm中写入文本文件涉及从一个Elm应用程序创建并保存文本数据到文件。程序员经常需要生成报告、日志，或以结构化文本格式（例如，JSON、CSV）导出数据以供其他应用程序使用或记录保持目的。然而，由于Elm的架构关注于纯净和安全，直接写文件——就像许多其他的副作用一样——是通过向周围的JavaScript环境发送命令来处理的。

## 如何操作：

由于Elm运行在浏览器中，并且旨在是一个没有副作用的纯编程语言，它没有直接访问文件系统的能力。因此，写入文件通常涉及通过端口(port)将数据发送给JavaScript。以下是你可以设置的方法：

1. **定义一个端口模块以发送文本数据给JavaScript：**

```elm
port module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)

-- 定义一个端口发送文本数据给JavaScript
port saveText : String -> Cmd msg

-- 主视图
view : Html msg
view =
    div []
        [ button [ onClick (saveText "Hello, Elm writes to a file!") ] [ text "保存到文件" ]
        ]

-- 订阅设置（在这个例子中未使用但对于一个端口模块是必须的）
subscriptions : model -> Sub msg
subscriptions _ =
    Sub.none

-- 应用程序设置
main : Program () model msg
main =
    Browser.element
        { init = \_ -> ((), Cmd.none)
        , view = \_ -> view
        , update = \_ _ -> ((), Cmd.none)
        , subscriptions = subscriptions
        }
```

2. **实现对应的JavaScript代码：**

在你的HTML文件或一个JavaScript模块中，处理Elm应用程序的端口以保存文本。你可以使用`FileSaver.js`库在客户端保存文件或将数据发送到服务器进行处理。

```javascript
// 假设已经调用了Elm.Main.init()且应用正在运行
app.ports.saveText.subscribe(function(text) {
    // 使用FileSaver.js在客户端保存文件
    var blob = new Blob([text], {type: "text/plain;charset=utf-8"});
    saveAs(blob, "example.txt");
});
```

示例输出因直接涉及到文件的创建而不直接适用，但在你的Elm应用程序中点击按钮后，应该会下载一个名为“example.txt”的文件到你的计算机，其中包含字符串“Hello, Elm writes to a file!”。

在这种方法中，Elm和JavaScript之间的通信至关重要。尽管Elm旨在包含尽可能多的应用程序逻辑，但通过端口与JavaScript的互操作使您可以执行Elm不直接支持的任务，如文件写入。记住，通过这种模式增强Elm的纯净和安全性，确保您的Elm应用程序保持易于维护和推理，即使它们与复杂的外部世界交互。
