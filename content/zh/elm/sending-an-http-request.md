---
title:                "发出 HTTP 请求"
date:                  2024-01-20T17:59:43.263543-07:00
model:                 gpt-4-1106-preview
simple_title:         "发出 HTTP 请求"

tag:                  "HTML and the Web"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elm/sending-an-http-request.md"
---

{{< edit_this_page >}}

## What & Why? 什么和为什么？
发送HTTP请求是网页与服务器间传信息的过程。程序员发送请求来获取数据、登陆账户、提交表单等。

## How to: 如何实现：
```Elm
import Http
import Json.Decode exposing (string)

type Msg = GotText String | RequestFailed Http.Error

getText : Cmd Msg
getText =
    Http.get
        { url = "https://api.example.com/data"
        , expect = Http.expectString GotText
        }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        GotText text ->
            ({ model | content = text }, Cmd.none)

        RequestFailed _ ->
            (model, Cmd.none)
```
当你执行`getText`，它会从提供的URL获取文本。`GotText`包含了响应，而`RequestFailed`包含了可能出现的错误。

## Deep Dive 深入了解：
发送HTTP请求是web开发的基石。Elm使用`Http`模块简化这一过程。Elm在0.18至今版本中，用`Task`到`Cmd`的转变，进一步简化HTTP请求处理。

其他语言如JavaScript有`fetch`和`XMLHttpRequest`。Elm中，使用`Http.expectString`处理纯文本响应；对于JSON，使用`Http.expectJson`。

Elm确保所有的HTTP请求都得到处理，因为每个请求结果都匹配一个`Msg`类型。这保证了代码的可靠性，减少了处理异步操作时可能出现的困难。

## See Also 参考链接：
- Elm HTTP package documentation: [https://package.elm-lang.org/packages/elm/http/latest/](https://package.elm-lang.org/packages/elm/http/latest/)
- Elm Guide - HTTP: [https://guide.elm-lang.org/effects/http.html](https://guide.elm-lang.org/effects/http.html)
- JSON Decoding in Elm: [https://guide.elm-lang.org/effects/json.html](https://guide.elm-lang.org/effects/json.html)
