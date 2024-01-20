---
title:                "发送http请求"
html_title:           "C#: 发送http请求"
simple_title:         "发送http请求"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elm/sending-an-http-request.md"
---

{{< edit_this_page >}}

# 使用Elm进行HTTP请求

## 什么和为什么？

HTTP请求是从你的应用到服务器的通信。我们使用它来发送或接收数据，比如从网络数据库获取信息。

## 如何操作：

下面是使用Elm（当前版本）发送HTTP GET请求并处理响应的代码示例。

```Elm
import Http
import Json.Decode as Decode

type alias User =
    { id : Int
    , name : String
    }

getUser : Int -> Cmd Msg
getUser userid =
    Http.get
        { url = "https://jsonplaceholder.typicode.com/users/" ++ String.fromInt userid
        , expect = Http.expectJson GotUser (Decode.field "name" Decode.string)
        }

type Msg
    = GotUser (Result Http.Error User)
```
上述代码通过URL获取用户，并将响应解码为`User`类型的实例。

## 深入详解：

Elm的HTTP模块基于XHR（XMLHttpRequest），这是一种web应用程序与服务器交互的技术。然而，为了处理效率，Elm封装了一个易于使用，更友好的API，以更有效地处理HTTP请求。

对于POST请求，你可以使用Http.post。Elm中还支持其他HTTP方法，比如PUT和DELETE。

在处理响应时，使用 `expectJson` 函数以及名为 `Json.Decode` 的模块，这些都基于JSON数据转换函数。

## 参考资料：

以下是一些相关的在线资源，可以帮助进一步理解和掌握Elm中的HTTP请求。

1. Elm的HTTP模块的官方文档: [https://package.elm-lang.org/packages/elm/http/latest/](https://package.elm-lang.org/packages/elm/http/latest/)
2. Elm的JSON解码器的官方文档: [https://package.elm-lang.org/packages/elm/json/latest/](https://package.elm-lang.org/packages/elm/json/latest/)
3. Elm官方教程: [https://guide.elm-lang.org/](https://guide.elm-lang.org/)