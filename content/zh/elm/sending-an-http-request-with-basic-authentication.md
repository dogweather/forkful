---
title:                "使用基本认证发送http请求"
html_title:           "Elm: 使用基本认证发送http请求"
simple_title:         "使用基本认证发送http请求"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elm/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# 为什么

在现代的互联网世界中，我们经常需要与不同的服务器进行通信，以获取或发送数据。通过发送 HTTP 请求，我们可以从远程服务器获取数据，并将其集成到我们的应用程序中。使用基本认证，我们还可以确保只有经过身份验证的用户才能访问敏感的服务器数据，保护我们的应用程序和用户信息的安全。

# 如何实现

发送带有基本认证的 HTTP 请求非常简单，只需要遵循以下几个步骤：

1. 导入 Elm 的 `Http` 模块
2. 创建一个带有用户名和密码的 `Authentication` 对象
3. 使用 `Http.send` 函数发送带有基本认证的请求
4. 处理服务器返回的响应数据

下面是一个简单的例子，演示如何发送带有基本认证的 HTTP 请求，并打印出服务器返回的响应：

```Elm
import Http
import Json.Decode exposing (Decoder)

type alias Authentication =
    { username : String
    , password : String
    }

type Msg
    = RequestSuccessful String
    | RequestFailed Http.Error

authentication : Authentication
authentication =
    { username = "username"
    , password = "password"
    }

url : String
url =
    "https://example.com/api/data"

decoder : Decoder String
decoder =
    Json.Decode.string

sendRequest : Cmd Msg
sendRequest =
    Http.send
        { method = "GET"
        , headers = []
        , url = url
        , body = Http.emptyBody
        , expect = Http.expectString RequestSuccessful decoder
        , timeout = Nothing
        , withCredentials = True
        , tracker = Nothing
        , auth = Just authentication
        }

```

在这个例子中，我们首先创建了一个 `Authentication` 对象，并声明了我们要访问的 URL。然后，我们通过使用 `Http.send` 函数发送请求，并指定我们想要的返回数据类型。最后，我们处理服务器返回的数据，如果请求成功，则打印出响应的内容，否则打印出错误信息。

# 深入了解

在实际应用程序中，我们可能需要发送带有参数的 HTTP 请求，或者使用其他认证方式，如 OAuth。在这种情况下，我们可以通过更改 `url` 和 `decoder` 变量来自定义我们的请求。同时，我们也可以使用 `Http.stringBody` 来发送带有参数的请求体，并且可以将 `auth` 设置为 `Nothing`，以发送不带认证的请求。更多关于 Elm 中发送 HTTP 请求的信息，可以查看官方文档。

# 查看更多

- [Elm 官方文档](https://elm-lang.org/docs)