---
date: 2024-01-20 17:59:50.035462-07:00
description: "How to: \u600E\u6837\u505A\uFF1F \u5728Haskell\u4E2D\uFF0C\u4F60\u53EF\
  \u4EE5\u4F7F\u7528`http-client`\u548C`http-conduit`\u5E93\u7B80\u5355\u5730\u53D1\
  \u9001HTTP\u8BF7\u6C42\u3002\u8FD9\u91CC\u6709\u4E00\u4E2A\u5C0F\u4F8B\u5B50\uFF1A\
  ."
isCJKLanguage: true
lastmod: '2024-04-05T21:53:48.124435-06:00'
model: gpt-4-1106-preview
summary: "\u600E\u6837\u505A\uFF1F \u5728Haskell\u4E2D\uFF0C\u4F60\u53EF\u4EE5\u4F7F\
  \u7528`http-client`\u548C`http-conduit`\u5E93\u7B80\u5355\u5730\u53D1\u9001HTTP\u8BF7\
  \u6C42\u3002\u8FD9\u91CC\u6709\u4E00\u4E2A\u5C0F\u4F8B\u5B50\uFF1A."
title: "\u53D1\u51FA HTTP \u8BF7\u6C42"
weight: 44
---

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
