---
date: 2024-01-20 18:01:42.730703-07:00
description: "\u5982\u4F55\u505A\uFF1A HTTP\u57FA\u672C\u8BA4\u8BC1\u7684\u65B9\u6CD5\
  \u7531\u5185\u7F6E\u4E8EHTTP\u6846\u67B6\u7684\u9A8C\u8BC1\u673A\u5236\u7EC4\u6210\
  \uFF0C\u5386\u53F2\u60A0\u4E45\uFF0C\u53EF\u4EE5\u8FFD\u8FF0\u81F3HTTP/1.0\u3002\
  \u5C3D\u7BA1\u57FA\u672C\u8BA4\u8BC1\u660E\u6587\u53D1\u9001\u7ECF\u8FC7Base64\u7F16\
  \u7801\u7684\u51ED\u636E\uFF0C\u5BB9\u6613\u53D7\u5230\u4E2D\u95F4\u4EBA\u653B\u51FB\
  \uFF0C\u4F46\u56E0\u5176\u7B80\u5355\u6027\uFF0C\u5728\u5185\u90E8\u6216\u8005\u4F4E\
  \u5B89\u5168\u9700\u6C42\u7684\u7CFB\u7EDF\u4E2D\u4ECD\u7136\u5E7F\u6CDB\u4F7F\u7528\
  \u3002\u9664\u6B64\u4E4B\u5916\uFF0C\u8FD8\u6709\u66F4\u5B89\u5168\u7684\u8BA4\u8BC1\
  \u65B9\u5F0F\uFF0C\u5982OAuth\u3002\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T22:38:46.977779-06:00'
model: gpt-4-1106-preview
summary: "\u5982\u4F55\u505A\uFF1A HTTP\u57FA\u672C\u8BA4\u8BC1\u7684\u65B9\u6CD5\u7531\
  \u5185\u7F6E\u4E8EHTTP\u6846\u67B6\u7684\u9A8C\u8BC1\u673A\u5236\u7EC4\u6210\uFF0C\
  \u5386\u53F2\u60A0\u4E45\uFF0C\u53EF\u4EE5\u8FFD\u8FF0\u81F3HTTP/1.0\u3002\u5C3D\
  \u7BA1\u57FA\u672C\u8BA4\u8BC1\u660E\u6587\u53D1\u9001\u7ECF\u8FC7Base64\u7F16\u7801\
  \u7684\u51ED\u636E\uFF0C\u5BB9\u6613\u53D7\u5230\u4E2D\u95F4\u4EBA\u653B\u51FB\uFF0C\
  \u4F46\u56E0\u5176\u7B80\u5355\u6027\uFF0C\u5728\u5185\u90E8\u6216\u8005\u4F4E\u5B89\
  \u5168\u9700\u6C42\u7684\u7CFB\u7EDF\u4E2D\u4ECD\u7136\u5E7F\u6CDB\u4F7F\u7528\u3002\
  \u9664\u6B64\u4E4B\u5916\uFF0C\u8FD8\u6709\u66F4\u5B89\u5168\u7684\u8BA4\u8BC1\u65B9\
  \u5F0F\uFF0C\u5982OAuth\u3002 Haskell\u4E2D\u53D1\u9001HTTP\u8BF7\u6C42\uFF0C\u901A\
  \u5E38\u4F7F\u7528\u5982`Network.HTTP.Simple`\u8FD9\u6837\u7684\u5E93\u6765\u7B80\
  \u5316\u6D41\u7A0B\u3002\u8FD9\u4E9B\u5E93\u5C01\u88C5\u4E86\u5E95\u5C42\u7F51\u7EDC\
  \u534F\u8BAE\uFF0C\u4F7F\u5F00\u53D1\u8005\u53EF\u4EE5\u66F4\u5BB9\u6613\u5730\u53D1\
  \u9001\u8BF7\u6C42\uFF0C\u5E76\u5904\u7406\u5982\u7F16\u7801\u3001\u8D85\u65F6\u548C\
  \u9519\u8BEF\u4E4B\u7C7B\u7684\u70B9\u3002"
title: "\u4F7F\u7528\u57FA\u672C\u8BA4\u8BC1\u53D1\u9001 HTTP \u8BF7\u6C42"
weight: 45
---

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
