---
title:                "发出 HTTP 请求"
date:                  2024-01-20T17:59:50.035462-07:00
model:                 gpt-4-1106-preview
simple_title:         "发出 HTTP 请求"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/haskell/sending-an-http-request.md"
---

{{< edit_this_page >}}

## What & Why? 什么与为什么？
发送HTTP请求是Web编程中的一项基本操作，它允许你从服务器获取或发送数据。程序员这么做主要是为了与远程服务交互，获取所需信息或触发特定操作。

## How to: 怎样做？
在Haskell中，你可以使用`http-client`和`http-conduit`库简单地发送HTTP请求。这里有一个小例子：

```haskell
import Network.HTTP.Simple

main :: IO ()
main = do
    response <- httpLBS "http://httpbin.org/get"
    putStrLn ("The status code was: " ++
               show (getResponseStatusCode response))
    print (getResponseHeader "Content-Type" response)
    putStrLn (unpack (decodeUtf8 (getResponseBody response)))
```

运行这段代码，你会得到类似下面的输出：

```
The status code was: 200
["application/json"]
{
  "args": {}, 
  "headers": {
    ...
  }, 
  "origin": "...", 
  "url": "https://httpbin.org/get"
}
```

## Deep Dive: 深入探索
早在1991年，HTTP协议就已经诞生了，而Haskell在1990年首次发布。随着时间的推移，Haskell社区开发了多个用于发送HTTP请求的库。`http-client`作为其中之一，提供了一个后端无关的HTTP客户端接口，而`http-conduit`则在此基础上实现了易用的流处理接口。

虽然Haskell的`http-client`和`http-conduit`库是发送HTTP请求的常见选择，但还有其他几个库如`wreq`和`req`也同样流行，它们提供了不同的抽象级别。

深入实现细节，这些库通常都是基于Haskell的网络底层库实现的，如`network`和`sockets`库。它们处理底层的TCP/IP连接，确保数据可以在网络中正确传输。

## See Also: 另请参阅
- [http-client package on Hackage](https://hackage.haskell.org/package/http-client) - Haskell的`http-client`库文档。
- [http-conduit package on Hackage](https://hackage.haskell.org/package/http-conduit) - Haskell的`http-conduit`库文档。
- [Haskell Network.HTTP.Simple documentation](https://hackage.haskell.org/package/http-conduit/docs/Network-HTTP-Simple.html) - `Network.HTTP.Simple`模块的详细文档。
- [Mozilla Developer Network — HTTP Guide](https://developer.mozilla.org/en-US/docs/Web/HTTP) - HTTP协议的详细信息和指南。
