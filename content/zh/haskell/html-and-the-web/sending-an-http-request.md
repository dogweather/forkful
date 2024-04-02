---
date: 2024-01-20 17:59:50.035462-07:00
description: "\u53D1\u9001HTTP\u8BF7\u6C42\u662FWeb\u7F16\u7A0B\u4E2D\u7684\u4E00\u9879\
  \u57FA\u672C\u64CD\u4F5C\uFF0C\u5B83\u5141\u8BB8\u4F60\u4ECE\u670D\u52A1\u5668\u83B7\
  \u53D6\u6216\u53D1\u9001\u6570\u636E\u3002\u7A0B\u5E8F\u5458\u8FD9\u4E48\u505A\u4E3B\
  \u8981\u662F\u4E3A\u4E86\u4E0E\u8FDC\u7A0B\u670D\u52A1\u4EA4\u4E92\uFF0C\u83B7\u53D6\
  \u6240\u9700\u4FE1\u606F\u6216\u89E6\u53D1\u7279\u5B9A\u64CD\u4F5C\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:47.811803-06:00'
model: gpt-4-1106-preview
summary: "\u53D1\u9001HTTP\u8BF7\u6C42\u662FWeb\u7F16\u7A0B\u4E2D\u7684\u4E00\u9879\
  \u57FA\u672C\u64CD\u4F5C\uFF0C\u5B83\u5141\u8BB8\u4F60\u4ECE\u670D\u52A1\u5668\u83B7\
  \u53D6\u6216\u53D1\u9001\u6570\u636E\u3002\u7A0B\u5E8F\u5458\u8FD9\u4E48\u505A\u4E3B\
  \u8981\u662F\u4E3A\u4E86\u4E0E\u8FDC\u7A0B\u670D\u52A1\u4EA4\u4E92\uFF0C\u83B7\u53D6\
  \u6240\u9700\u4FE1\u606F\u6216\u89E6\u53D1\u7279\u5B9A\u64CD\u4F5C\u3002"
title: "\u53D1\u51FA HTTP \u8BF7\u6C42"
weight: 44
---

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
