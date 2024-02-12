---
title:                "使用基本认证发送 HTTP 请求"
aliases:
- /zh/haskell/sending-an-http-request-with-basic-authentication.md
date:                  2024-01-20T18:01:42.730703-07:00
model:                 gpt-4-1106-preview
simple_title:         "使用基本认证发送 HTTP 请求"

tag:                  "HTML and the Web"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/haskell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？
发送带有基本认证的HTTP请求是一种网上通信方法，用于在客户端和服务器间安全传输用户名和密码。程序员这样做是为了访问需要验证的资源，比如APIs。

## 如何做：
```Haskell
import Network.HTTP.Simple

-- 设置请求URL和认证信息
let url = "http://example.com/data"
let auth = basicAuth "username" "password"

-- 创建请求并添加认证
let request = setRequestURL url $ setRequestBasicAuth auth defaultRequest

-- 发送请求并获取响应
response <- httpLBS request

-- 打印响应的状态码和正文
putStrLn $ "Status code: " ++ show (getResponseStatusCode response)
putStrLn $ "Body: " ++ show (getResponseBody response)
```
样例输出：
```
Status code: 200
Body: <response body>
```

## 深入了解
HTTP基本认证的方法由内置于HTTP框架的验证机制组成，历史悠久，可以追述至HTTP/1.0。尽管基本认证明文发送经过Base64编码的凭据，容易受到中间人攻击，但因其简单性，在内部或者低安全需求的系统中仍然广泛使用。除此之外，还有更安全的认证方式，如OAuth。

Haskell中发送HTTP请求，通常使用如`Network.HTTP.Simple`这样的库来简化流程。这些库封装了底层网络协议，使开发者可以更容易地发送请求，并处理如编码、超时和错误之类的点。

## 参考链接
- Haskell `http-conduit` [官方文档](https://www.stackage.org/package/http-conduit)
- HTTP认证概述 [MDN Web Docs](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)
- Base64编码解码 [Wiki](https://en.wikipedia.org/wiki/Base64)
