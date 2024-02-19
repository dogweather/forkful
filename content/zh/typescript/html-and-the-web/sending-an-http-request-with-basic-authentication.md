---
aliases:
- /zh/typescript/sending-an-http-request-with-basic-authentication/
date: 2024-01-20 18:02:35.587785-07:00
description: "\u5728TypeScript\u4E2D\uFF0C\u53D1\u9001\u5E26\u6709\u57FA\u672C\u8BA4\
  \u8BC1\u7684HTTP\u8BF7\u6C42\uFF0C\u610F\u5473\u7740\u5C06\u7528\u6237\u540D\u548C\
  \u5BC6\u7801\u52A0\u5BC6\u540E\u4F5C\u4E3A\u8BF7\u6C42\u5934\u90E8\u53D1\u9001\uFF0C\
  \u4EE5\u5B9E\u73B0\u7528\u6237\u8BA4\u8BC1\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\
  \u662F\u4E3A\u4E86\u5B89\u5168\u5730\u8BBF\u95EE\u53D7\u4FDD\u62A4\u7684\u8D44\u6E90\
  \u3002"
isCJKLanguage: true
lastmod: 2024-02-18 23:08:58.903645
model: gpt-4-1106-preview
summary: "\u5728TypeScript\u4E2D\uFF0C\u53D1\u9001\u5E26\u6709\u57FA\u672C\u8BA4\u8BC1\
  \u7684HTTP\u8BF7\u6C42\uFF0C\u610F\u5473\u7740\u5C06\u7528\u6237\u540D\u548C\u5BC6\
  \u7801\u52A0\u5BC6\u540E\u4F5C\u4E3A\u8BF7\u6C42\u5934\u90E8\u53D1\u9001\uFF0C\u4EE5\
  \u5B9E\u73B0\u7528\u6237\u8BA4\u8BC1\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\u662F\
  \u4E3A\u4E86\u5B89\u5168\u5730\u8BBF\u95EE\u53D7\u4FDD\u62A4\u7684\u8D44\u6E90\u3002"
title: "\u4F7F\u7528\u57FA\u672C\u8BA4\u8BC1\u53D1\u9001 HTTP \u8BF7\u6C42"
---

{{< edit_this_page >}}

## What & Why? (什么及为什么?)
在TypeScript中，发送带有基本认证的HTTP请求，意味着将用户名和密码加密后作为请求头部发送，以实现用户认证。程序员这样做是为了安全地访问受保护的资源。

## How to: (如何操作:)
```TypeScript
import axios from 'axios';

const url = 'https://api.example.com/data';
const username = 'yourUsername';
const password = 'yourPassword';

const basicAuth = 'Basic ' + btoa(username + ':' + password);

axios.get(url, {
  headers: { 'Authorization': basicAuth }
})
.then(response => {
  console.log('Data retrieved:', response.data);
})
.catch(error => {
  console.error('Error fetching data:', error);
});
```

Sample output:
```
Data retrieved: { /* ...response data... */ }
```

## Deep Dive (深入探讨)
发送带基本认证的HTTP请求是早期认证方式。它简单但不是最安全的，因为基本认证可被中间人攻击。所以，总是在HTTPS上使用它。现在，有更安全的替代方法，如OAuth和JWT。在构建`axios`请求时，要编码用户名和密码，使用`btoa`函数。然而，最近的浏览器环境和Node.js有内置方法处理这种编码问题。

## See Also (另请参阅)
- MDN 关于 HTTP 基本认证: https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication
- Node.js 如何处理基本认证: https://nodejs.dev/learn/authenticating-nodejs-requests
- HTTP 安全最佳实践: https://owasp.org/www-project-top-ten/
- `axios` 库文档: https://axios-http.com/docs/intro
- JWT 认证机制: https://jwt.io/introduction/
- OAuth 官方网站: https://oauth.net/
