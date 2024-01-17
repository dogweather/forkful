---
title:                "发送一个http请求"
html_title:           "Haskell: 发送一个http请求"
simple_title:         "发送一个http请求"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/haskell/sending-an-http-request.md"
---

{{< edit_this_page >}}

# "什么是HTTP请求？为什么程序员要使用它？"

在编程中，发送HTTP请求是指向某个网络服务器发送请求，以获取或传输信息的过程。程序员通常会使用HTTP请求来与其他服务器上的应用程序进行交互，例如从一个API获取数据或向一个网页发送数据。

# "如何操作："
```Haskell
import Network.HTTP
import Network.URI

main = do
  -- 创建一个简单的GET请求
  response <- simpleHTTP (getRequest "http://www.example.com")
  -- 获取响应的内容
  responseBody <- getResponseBody response
  -- 打印出响应内容
  putStrLn responseBody
  
  -- 创建一个带有请求头的GET请求
  let request = Request {rqURI = fromJust $ parseURI "http://www.example.com",
                         rqMethod = GET,
                         rqHeaders = [mkHeader HdrUserAgent "MyCustomUserAgent"],
                         rqBody = ""}
  -- 发送请求
  response2 <- simpleHTTP request
  -- 获取响应的内容
  responseBody2 <- getResponseBody response2
  -- 打印出响应内容
  putStrLn responseBody2
```

# "深入探讨："
HTTP请求是基于客户端-服务器架构的一种通信协议，它被用于在客户端和服务器之间传输数据。它由Tim Berners-Lee在1989年设计并在1996年正式发布。HTTP请求通常有两种方法：GET方法用于获取数据，POST方法用于向服务器发送数据。除了Haskell，其他编程语言也支持发送HTTP请求，例如Python中的Requests库和Java中的HttpURLConnection类。

# "相关网址："
- [官方Haskell文档关于HTTP请求的说明](https://hackage.haskell.org/package/HTTP-4000.3.15/docs/Network-HTTP.html)
- [Tim Berners-Lee关于HTTP的原始论文](https://tools.ietf.org/html/rfc1945)