---
title:                "Elm: 使用基本身份验证发送http请求"
simple_title:         "使用基本身份验证发送http请求"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elm/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# 为什么

HTTP请求和基本认证是在网络编程中经常遇到的一种情况。使用基本认证可以确保数据的安全性，并允许访问受限资源。在Elm中发送HTTP请求并进行基本认证是一种非常常见的任务，因此我们需要了解如何做到这一点。

## 怎么做

首先，我们需要导入`elm/http`库，它提供了发送HTTP请求的函数。接下来，我们需要设置请求的URL和所需的认证信息。最后，我们可以使用`Http.request`函数来发送请求，并在`Http.expectStringResponse`中指定预期的响应类型。下面是一个简单的例子：

```
import Http
import Json.Decode exposing (Decoder, string)
 
-- 创建一个认证头
authorizationHeader : String
authorizationHeader =
  "Basic " ++ (Http.base64Encode "username:password")
 
-- 创建请求的URL
url : String
url = "https://example.com/api/resource"
 
-- 定义期望的响应类型
responseDecoder : Decoder String
responseDecoder = string
 
-- 发送HTTP请求
request : Cmd Msg
request =
  Http.request
   { method = "GET"
   , headers = [ ( "Authorization", authorizationHeader ) ]
   , url = url
   , expect = Http.expectStringResponse responseDecoder 
   , timeout = Nothing
   , tracker = Nothing
   }
```

在这个例子中，我们首先创建了一个认证头，使用`Http.base64Encode`函数将用户名与密码进行编码。然后，我们设置了请求的URL和期望的响应类型为字符串。最后，我们调用`Http.request`函数，并将返回的Cmd作为更新函数的结果，这样我们就可以将请求发送到服务器并等待响应。

## 深入了解

当我们发送HTTP请求时，通常会遇到一些错误。在这种情况下，服务器会返回一个响应，其中包含HTTP状态码和一些错误信息。在这种情况下，我们可以使用`Http.expectStringResponse`中的`Http.BadStatus`函数来捕获错误并进行处理。我们也可以使用其他的`expect`函数来处理不同类型的响应，例如JSON对象或二进制数据。

此外，我们还可以在请求中附加其他的参数，例如`timeout`来设置请求的超时时间，或者`tracker`来跟踪请求的进度。更多关于HTTP请求的深入知识，请参阅官方文档。

# 参考链接

- Elm官方文档：https://guide.elm-lang.org/
- Http库文档：https://package.elm-lang.org/packages/elm/http/latest/
- Http请求的基本认证：https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication#Basic_authentication_scheme