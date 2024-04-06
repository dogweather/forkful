---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:27:58.437653-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u7531\u4E8EElm\u8FD0\u884C\u5728\u6D4F\
  \u89C8\u5668\u4E2D\uFF0C\u5E76\u4E14\u65E8\u5728\u662F\u4E00\u4E2A\u6CA1\u6709\u526F\
  \u4F5C\u7528\u7684\u7EAF\u7F16\u7A0B\u8BED\u8A00\uFF0C\u5B83\u6CA1\u6709\u76F4\u63A5\
  \u8BBF\u95EE\u6587\u4EF6\u7CFB\u7EDF\u7684\u80FD\u529B\u3002\u56E0\u6B64\uFF0C\u5199\
  \u5165\u6587\u4EF6\u901A\u5E38\u6D89\u53CA\u901A\u8FC7\u7AEF\u53E3(port)\u5C06\u6570\
  \u636E\u53D1\u9001\u7ED9JavaScript\u3002\u4EE5\u4E0B\u662F\u4F60\u53EF\u4EE5\u8BBE\
  \u7F6E\u7684\u65B9\u6CD5\uFF1A 1. **\u5B9A\u4E49\u4E00\u4E2A\u7AEF\u53E3\u6A21\u5757\
  \u4EE5\u53D1\u9001\u6587\u672C\u6570\u636E\u7ED9JavaScript\uFF1A**."
lastmod: '2024-04-05T22:38:46.855276-06:00'
model: gpt-4-0125-preview
summary: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u7531\u4E8EElm\u8FD0\u884C\u5728\u6D4F\u89C8\
  \u5668\u4E2D\uFF0C\u5E76\u4E14\u65E8\u5728\u662F\u4E00\u4E2A\u6CA1\u6709\u526F\u4F5C\
  \u7528\u7684\u7EAF\u7F16\u7A0B\u8BED\u8A00\uFF0C\u5B83\u6CA1\u6709\u76F4\u63A5\u8BBF\
  \u95EE\u6587\u4EF6\u7CFB\u7EDF\u7684\u80FD\u529B\u3002\u56E0\u6B64\uFF0C\u5199\u5165\
  \u6587\u4EF6\u901A\u5E38\u6D89\u53CA\u901A\u8FC7\u7AEF\u53E3(port)\u5C06\u6570\u636E\
  \u53D1\u9001\u7ED9JavaScript\u3002\u4EE5\u4E0B\u662F\u4F60\u53EF\u4EE5\u8BBE\u7F6E\
  \u7684\u65B9\u6CD5\uFF1A 1. **\u5B9A\u4E49\u4E00\u4E2A\u7AEF\u53E3\u6A21\u5757\u4EE5\
  \u53D1\u9001\u6587\u672C\u6570\u636E\u7ED9JavaScript\uFF1A**."
title: "\u7F16\u5199\u6587\u672C\u6587\u4EF6"
weight: 24
---

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
