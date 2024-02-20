---
date: 2024-01-20 18:02:03.876575-07:00
description: "\u53D1\u9001\u5E26\u6709\u57FA\u672C\u8BA4\u8BC1\u7684HTTP\u8BF7\u6C42\
  \u53EF\u4EE5\u8BA9\u4F60\u8BBF\u95EE\u9700\u8981\u9A8C\u8BC1\u7684\u8D44\u6E90\u3002\
  \u7A0B\u5E8F\u5458\u8FD9\u4E48\u505A\u662F\u4E3A\u4E86\u786E\u4FDD\u5B89\u5168\u6027\
  \uFF0C\u53EA\u5141\u8BB8\u7ECF\u8FC7\u6388\u6743\u7684\u4EBA\u8BBF\u95EE\u654F\u611F\
  \u6570\u636E\u3002"
isCJKLanguage: true
lastmod: 2024-02-19 22:05:07.267243
model: gpt-4-1106-preview
summary: "\u53D1\u9001\u5E26\u6709\u57FA\u672C\u8BA4\u8BC1\u7684HTTP\u8BF7\u6C42\u53EF\
  \u4EE5\u8BA9\u4F60\u8BBF\u95EE\u9700\u8981\u9A8C\u8BC1\u7684\u8D44\u6E90\u3002\u7A0B\
  \u5E8F\u5458\u8FD9\u4E48\u505A\u662F\u4E3A\u4E86\u786E\u4FDD\u5B89\u5168\u6027\uFF0C\
  \u53EA\u5141\u8BB8\u7ECF\u8FC7\u6388\u6743\u7684\u4EBA\u8BBF\u95EE\u654F\u611F\u6570\
  \u636E\u3002"
title: "\u4F7F\u7528\u57FA\u672C\u8BA4\u8BC1\u53D1\u9001 HTTP \u8BF7\u6C42"
---

{{< edit_this_page >}}

## What & Why? (是什么？为什么？)
发送带有基本认证的HTTP请求可以让你访问需要验证的资源。程序员这么做是为了确保安全性，只允许经过授权的人访问敏感数据。

## How to: (如何实现)
```Javascript
// 使用Node.js的内置https模块发送带基本认证的HTTP请求
const https = require('https');

// 编码你的用户名和密码到Base64格式
const base64Credentials = Buffer.from('username:password').toString('base64');

// 设置HTTP请求头
const options = {
  hostname: 'example.com',
  port: 443,
  path: '/your-protected-resource',
  method: 'GET',
  headers: {
    'Authorization': `Basic ${base64Credentials}`
  }
};

// 发送请求
const req = https.request(options, (res) => {
  console.log(`状态码: ${res.statusCode}`);

  res.on('data', (d) => {
    process.stdout.write(d);
  });
});

// 错误处理
req.on('error', (e) => {
  console.error(e);
});

req.end();
```
输出示例:
```
状态码: 200
```
## Deep Dive (深入研究)
基本认证是HTTP协议中历史悠久的一部分，基于RFC 7617标准。尽管简单，但不适合高安全需求，因为Base64编码非加密，可逆。现代应用常用更安全的认证方式，如OAuth。执行时，记得使用HTTPS以防中间人攻击，保持传输加密。

## See Also (另请参阅)
- MDN web docs on HTTP authentication: https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication
- Node.js `https` module documentation: https://nodejs.org/api/https.html
- RFC 7617, The 'Basic' HTTP Authentication Scheme: https://tools.ietf.org/html/rfc7617
