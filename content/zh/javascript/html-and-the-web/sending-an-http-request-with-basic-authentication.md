---
date: 2024-01-20 18:02:03.876575-07:00
description: "How to: (\u5982\u4F55\u5B9E\u73B0) \u8F93\u51FA\u793A\u4F8B."
isCJKLanguage: true
lastmod: '2024-04-05T21:53:48.492539-06:00'
model: gpt-4-1106-preview
summary: ''
title: "\u4F7F\u7528\u57FA\u672C\u8BA4\u8BC1\u53D1\u9001 HTTP \u8BF7\u6C42"
weight: 45
---

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
