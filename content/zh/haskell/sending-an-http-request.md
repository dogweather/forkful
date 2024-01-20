---
title:                "发送http请求"
html_title:           "C#: 发送http请求"
simple_title:         "发送http请求"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/haskell/sending-an-http-request.md"
---

{{< edit_this_page >}}

# Haskell中的HTTP请求：一窥其奥秘

## 什么以及为什么？

HTTP请求是一种协议，使得程序可以通过网络从服务端获取数据或发送数据给服务端。程序员之所以使用，是因为这是Web交互的基础。

## 如何实施？

在Haskell中，你可以使用HTTP库，具体如下：

```Haskell
import Network.HTTP

main = do
  response <- simpleHTTP (getRequest "http://httpbin.org/get")
  putStrLn =<< getResponseBody response
```

我们首先导入`Network.HTTP`模块，然后使用`simpleHTTP`函数和`getRequest`建立HTTP GET请求。接着我们处理响应，打印出服务器的响应体。

## 深度探索

### 历史背景

Haskell的HTTP库在早期的开发中就已经存在，它提供了一个操作简单但功能齐全的HTTP客户端接口。

### 可选方法

虽然这个库在大部分使用场景下都能胜任，但也有一些其他的库提供了更多高级的功能，比如`http-conduit`和`wreq`。

### 实现细节

`simpleHTTP`函数实际上是执行了一个`Request`类型的参数，并返回一个`IO Response`，返回的是服务器的响应。`Request`是描述HTTP请求的数据类型，而`Response`是描述HTTP响应的数据类型。

## 参见
- [Haskell HTTP库文档](https://hackage.haskell.org/package/HTTP)
- [http-conduit库](https://hackage.haskell.org/package/http-conduit)
- [wreq库](https://hackage.haskell.org/package/wreq)