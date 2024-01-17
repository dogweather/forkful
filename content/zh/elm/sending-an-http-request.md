---
title:                "发送http请求"
html_title:           "Elm: 发送http请求"
simple_title:         "发送http请求"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elm/sending-an-http-request.md"
---

{{< edit_this_page >}}

## 什么是HTTP请求及其用途？
发送HTTP请求是通过互联网与服务器进行通信的一种方式。程序员经常发送HTTP请求来获取网页、数据或其他信息，这样他们就能够在自己的应用程序中使用这些信息。

## 如何实现：
```elm
import Http exposing (..)
import Json.Decode exposing (..)

getPosts : Cmd Msg
getPosts =
    let
        url = "https://jsonplaceholder.typicode.com/posts"
    in
        send GetPosts (get url decodePosts)

type Msg
    = GetPosts (Result Http.Error (List Post))

type alias Post =
    { userId : Int
    , id : Int
    , title : String
    , body : String
    }

decodePosts : Decoder (List Post)
decodePosts =
    list (
        map4 Post
        (field "userId" int)
        (field "id" int)
        (field "title" string)
        (field "body" string)
    )
```

## 深入了解：
HTTP是一种通用的协议，用于客户端和服务器之间的通信。它可以使用不同的方法（GET、POST等）来传输数据，还可以通过头部信息来指定一些其他的信息。除了使用Elm自带的Http模块，还可以使用第三方库如elm-http-builder来构建请求。

## 参考资料：
- [Elm官方文档](https://guide.elm-lang.org/)
- [Elm-http-builder库](https://package.elm-lang.org/packages/ktonon/elm-http-builder/latest/)
- [HTTP协议的历史背景](https://developer.mozilla.org/zh-CN/docs/Web/HTTP/Overview)