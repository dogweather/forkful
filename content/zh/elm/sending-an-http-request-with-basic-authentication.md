---
title:                "使用基本身份验证发送http请求"
html_title:           "Elm: 使用基本身份验证发送http请求"
simple_title:         "使用基本身份验证发送http请求"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elm/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 什么&为什么?

发送HTTP请求是一种通过网络传输信息的常见方式。程序员经常使用它来与外部服务通信，从而实现更复杂的功能。

## 如何:

```Elm
ExampleCode basicAuthRequest = 
    Http.send BasicAuthRequest
        { method = "GET",
          url = "https://example.com/api/users",
          headers = [ ("Authorization", "Basic dXNlcjpwYXNzd29yZA==") ],
          body = Http.emptyBody,
          expect = Http.expectJson NoOp myDecoder }
```

发送HTTP请求需要组装一个`BasicAuthRequest`，其中包含方法、URL、头信息、请求体和预期结果的定义。在头信息中，必须包含带有base64编码的用户名和密码的`Authorization`头。完成后，使用`Http.send`函数将请求发送到指定的URL。

## 深入讨论:

基本认证是一种最古老的身份验证机制，它可以追溯到Web的早期发展。现在，它已被先进且更安全的身份验证方法所取代，但仍然被一些服务使用。在发送HTTP请求时，有许多其他选项可供选择，例如OAuth或JWT。

关于发送HTTP请求的实现细节，可以深入了解`elm/http`包的操作原理。还可以使用`elm/json`包来解析返回的JSON数据。

## 查看更多:

- [Elm`http`包文档](https://package.elm-lang.org/packages/elm/http/latest/)
- [Elm`json`包文档](https://package.elm-lang.org/packages/elm/json/latest/)
- [基础认证](https://developer.mozilla.org/zh-CN/docs/Web/HTTP/Authentication)