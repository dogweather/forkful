---
title:                "Elm: 发送HTTP请求"
simple_title:         "发送HTTP请求"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elm/sending-an-http-request.md"
---

{{< edit_this_page >}}

## 为什么

在现代的网络应用程序中，我们经常需要与后端服务器通信获取数据。这时候发送HTTP请求就变得非常重要，它可以让我们轻松地在Elm中获取数据并将其展示给用户。

## 如何执行

首先，我们需要使用Elm的Http模块，它提供了发送和接收HTTP请求的功能。我们可以使用命令 "elm install elm/http" 来安装它。然后，在程序中导入该模块，然后就可以开始发送HTTP请求了。

```Elm
import Http

-- 假设我们想获取一个用户的信息，我们可以这样发送一个GET请求
getUserInfo : Cmd Msg
getUserInfo =
    let
        url = "https://example.com/user?id=123"
        decoder = Json.Decode.string
    in
    Http.getString url decoder
```

这里我们传入了请求的URL和相应数据的解码器，这里我们使用了Json.Decode.string来解析字符串类型的数据。如果我们想发送一个带有请求体的POST请求，可以使用Http.post函数。

```Elm
import Http

-- 假设我们想更新一个用户的信息，我们可以这样发送一个POST请求
updateUserInfo : String -> Cmd Msg
updateUserInfo newName =
    let
        url = "https://example.com/user/update"
        body = Http.jsonBody <| Json.Encode.object
            [ ( "name", Json.Encode.string newName ) ]
    in
    Http.post url body decodedUserInfo

decodedUserInfo : Decoder String
decodedUserInfo =
    Json.Decode.map (\obj -> "Updated successfully") Json.Decode.value
```

在这里，我们使用了Http.jsonBody函数来构造一个JSON请求体，并传入相应的解码器来解析服务器返回的数据。在发送请求时，我们还需要处理可能发生的错误，可以通过使用Http.send命令来发送请求并处理响应。

```Elm
-- 假设我们想在用户信息更新成功后显示一个提示信息，
-- 可以这样定义消息类型来处理请求成功和失败的情况
type Msg
    = UpdateSuccess String
    | UpdateError Http.Error

updateUserInfo : String -> Cmd Msg
updateUserInfo newName =
    let
        url = "https://example.com/user/update"
        body = Http.jsonBody <| Json.Encode.object
            [ ( "name", Json.Encode.string newName ) ]
    in
    Http.send UpdateSuccess UpdateError <| Http.post url body decodedUserInfo
```

## 深入了解

当我们发送HTTP请求时，还有一些额外的设置可以帮助我们更有效地管理请求。比如，我们可以设置请求的超时时间、请求头信息等。具体可以参考官方文档中关于Http模块的详细介绍。

## 参考链接

- 官方文档：https://guide.elm-lang.org/effects/http.html
- Http模块：https://package.elm-lang.org/packages/elm/http/latest/Http
- Json.Decode模块：https://package.elm-lang.org/packages/elm/json/latest/Json-Decode