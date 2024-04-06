---
date: 2024-01-20 18:02:35.587785-07:00
description: ''
isCJKLanguage: true
lastmod: '2024-04-05T22:51:00.674251-06:00'
model: gpt-4-1106-preview
summary: "(\u5982\u4F55\u64CD\u4F5C:) \u53D1\u9001\u5E26\u57FA\u672C\u8BA4\u8BC1\u7684\
  HTTP\u8BF7\u6C42\u662F\u65E9\u671F\u8BA4\u8BC1\u65B9\u5F0F\u3002\u5B83\u7B80\u5355\
  \u4F46\u4E0D\u662F\u6700\u5B89\u5168\u7684\uFF0C\u56E0\u4E3A\u57FA\u672C\u8BA4\u8BC1\
  \u53EF\u88AB\u4E2D\u95F4\u4EBA\u653B\u51FB\u3002\u6240\u4EE5\uFF0C\u603B\u662F\u5728\
  HTTPS\u4E0A\u4F7F\u7528\u5B83\u3002\u73B0\u5728\uFF0C\u6709\u66F4\u5B89\u5168\u7684\
  \u66FF\u4EE3\u65B9\u6CD5\uFF0C\u5982OAuth\u548CJWT\u3002\u5728\u6784\u5EFA`axios`\u8BF7\
  \u6C42\u65F6\uFF0C\u8981\u7F16\u7801\u7528\u6237\u540D\u548C\u5BC6\u7801\uFF0C\u4F7F\
  \u7528`btoa`\u51FD\u6570\u3002\u7136\u800C\uFF0C\u6700\u8FD1\u7684\u6D4F\u89C8\u5668\
  \u73AF\u5883\u548CNode.js\u6709\u5185\u7F6E\u65B9\u6CD5\u5904\u7406\u8FD9\u79CD\u7F16\
  \u7801\u95EE\u9898\u3002"
title: "\u4F7F\u7528\u57FA\u672C\u8BA4\u8BC1\u53D1\u9001 HTTP \u8BF7\u6C42"
weight: 45
---

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
