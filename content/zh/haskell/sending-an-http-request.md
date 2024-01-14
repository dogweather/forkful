---
title:                "Haskell: 发送http请求"
simple_title:         "发送http请求"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/haskell/sending-an-http-request.md"
---

{{< edit_this_page >}}

为什么：为了实现网络通信，我们需要发送HTTP请求。这是一种简单但强大的方法，可以将数据从一个服务器发送到另一个服务器。

## 如何发送HTTP请求

```Haskell
import Network.HTTP

main = do
    -- 创建一个简单的GET请求
    response <- simpleHTTP (getRequest "https://www.example.com")

    -- 从响应中提取数据
    responseBody <- getResponseBody response

    -- 打印响应内容
    print responseBody
```

以上代码首先导入了Haskell的网络库`Network.HTTP`，然后使用`simpleHTTP`函数创建一个GET请求，指定了要发送的URL。接着使用`getResponseBody`函数从响应中提取出内容，并打印到控制台上。

输出结果可能是一个HTML页面的源代码，或者是一个JSON格式的数据。发送HTTP请求可以让我们获取来自不同服务器的任何信息，并在我们的代码中使用它们。

## 深入了解发送HTTP请求

发送HTTP请求可以实现多种功能，例如：

- 使用不同的请求方法，例如POST、PUT、DELETE等
- 添加请求头（headers）
- 使用身份验证（authentication）来访问受限资源
- 处理重定向（redirects）

在更复杂的Haskell程序中，我们可以使用功能更强大的网络库来发送HTTP请求，例如`HTTP.Conduit`或`wreq`。这些库提供了更多的选项和功能，可以帮助我们更轻松地处理网络通信。

## 参考链接

- [Haskell网络库 - Network.HTTP文档](https://hackage.haskell.org/package/HTTP-4000.3.15/docs/Network-HTTP.html)
- [HTTP请求方法 - HTTP协议文档](https://www.runoob.com/http/http-methods.html)
- [Haskell网络库 - HTTP.Conduit文档](https://hackage.haskell.org/package/http-conduit)
- [Haskell网络库 - wreq文档](https://hackage.haskell.org/package/wreq)