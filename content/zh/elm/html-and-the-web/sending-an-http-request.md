---
date: 2024-01-20 17:59:43.263543-07:00
description: "How to: \u5982\u4F55\u5B9E\u73B0\uFF1A \u5F53\u4F60\u6267\u884C`getText`\uFF0C\
  \u5B83\u4F1A\u4ECE\u63D0\u4F9B\u7684URL\u83B7\u53D6\u6587\u672C\u3002`GotText`\u5305\
  \u542B\u4E86\u54CD\u5E94\uFF0C\u800C`RequestFailed`\u5305\u542B\u4E86\u53EF\u80FD\
  \u51FA\u73B0\u7684\u9519\u8BEF\u3002"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:47.986817-06:00'
model: gpt-4-1106-preview
summary: "\u5982\u4F55\u5B9E\u73B0\uFF1A \u5F53\u4F60\u6267\u884C`getText`\uFF0C\u5B83\
  \u4F1A\u4ECE\u63D0\u4F9B\u7684URL\u83B7\u53D6\u6587\u672C\u3002`GotText`\u5305\u542B\
  \u4E86\u54CD\u5E94\uFF0C\u800C`RequestFailed`\u5305\u542B\u4E86\u53EF\u80FD\u51FA\
  \u73B0\u7684\u9519\u8BEF\u3002"
title: "\u53D1\u51FA HTTP \u8BF7\u6C42"
weight: 44
---

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
