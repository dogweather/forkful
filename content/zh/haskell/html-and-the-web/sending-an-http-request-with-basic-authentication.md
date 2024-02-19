---
aliases:
- /zh/haskell/sending-an-http-request-with-basic-authentication/
date: 2024-01-20 18:01:42.730703-07:00
description: "\u53D1\u9001\u5E26\u6709\u57FA\u672C\u8BA4\u8BC1\u7684HTTP\u8BF7\u6C42\
  \u662F\u4E00\u79CD\u7F51\u4E0A\u901A\u4FE1\u65B9\u6CD5\uFF0C\u7528\u4E8E\u5728\u5BA2\
  \u6237\u7AEF\u548C\u670D\u52A1\u5668\u95F4\u5B89\u5168\u4F20\u8F93\u7528\u6237\u540D\
  \u548C\u5BC6\u7801\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\u662F\u4E3A\u4E86\u8BBF\
  \u95EE\u9700\u8981\u9A8C\u8BC1\u7684\u8D44\u6E90\uFF0C\u6BD4\u5982APIs\u3002"
isCJKLanguage: true
lastmod: 2024-02-18 23:08:59.169740
model: gpt-4-1106-preview
summary: "\u53D1\u9001\u5E26\u6709\u57FA\u672C\u8BA4\u8BC1\u7684HTTP\u8BF7\u6C42\u662F\
  \u4E00\u79CD\u7F51\u4E0A\u901A\u4FE1\u65B9\u6CD5\uFF0C\u7528\u4E8E\u5728\u5BA2\u6237\
  \u7AEF\u548C\u670D\u52A1\u5668\u95F4\u5B89\u5168\u4F20\u8F93\u7528\u6237\u540D\u548C\
  \u5BC6\u7801\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\u662F\u4E3A\u4E86\u8BBF\u95EE\
  \u9700\u8981\u9A8C\u8BC1\u7684\u8D44\u6E90\uFF0C\u6BD4\u5982APIs\u3002"
title: "\u4F7F\u7528\u57FA\u672C\u8BA4\u8BC1\u53D1\u9001 HTTP \u8BF7\u6C42"
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
