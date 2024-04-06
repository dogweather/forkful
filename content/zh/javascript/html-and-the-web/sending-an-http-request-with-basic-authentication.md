---
date: 2024-01-20 18:02:03.876575-07:00
description: "How to: (\u5982\u4F55\u5B9E\u73B0) \u57FA\u672C\u8BA4\u8BC1\u662FHTTP\u534F\
  \u8BAE\u4E2D\u5386\u53F2\u60A0\u4E45\u7684\u4E00\u90E8\u5206\uFF0C\u57FA\u4E8ERFC\
  \ 7617\u6807\u51C6\u3002\u5C3D\u7BA1\u7B80\u5355\uFF0C\u4F46\u4E0D\u9002\u5408\u9AD8\
  \u5B89\u5168\u9700\u6C42\uFF0C\u56E0\u4E3ABase64\u7F16\u7801\u975E\u52A0\u5BC6\uFF0C\
  \u53EF\u9006\u3002\u73B0\u4EE3\u5E94\u7528\u5E38\u7528\u66F4\u5B89\u5168\u7684\u8BA4\
  \u8BC1\u65B9\u5F0F\uFF0C\u5982OAuth\u3002\u6267\u884C\u65F6\uFF0C\u8BB0\u5F97\u4F7F\
  \u7528HTTPS\u4EE5\u9632\u4E2D\u95F4\u4EBA\u653B\u51FB\uFF0C\u4FDD\u6301\u4F20\u8F93\
  \u52A0\u5BC6\u3002"
isCJKLanguage: true
lastmod: '2024-04-05T22:51:01.412244-06:00'
model: gpt-4-1106-preview
summary: "(\u5982\u4F55\u5B9E\u73B0) \u57FA\u672C\u8BA4\u8BC1\u662FHTTP\u534F\u8BAE\
  \u4E2D\u5386\u53F2\u60A0\u4E45\u7684\u4E00\u90E8\u5206\uFF0C\u57FA\u4E8ERFC 7617\u6807\
  \u51C6\u3002\u5C3D\u7BA1\u7B80\u5355\uFF0C\u4F46\u4E0D\u9002\u5408\u9AD8\u5B89\u5168\
  \u9700\u6C42\uFF0C\u56E0\u4E3ABase64\u7F16\u7801\u975E\u52A0\u5BC6\uFF0C\u53EF\u9006\
  \u3002\u73B0\u4EE3\u5E94\u7528\u5E38\u7528\u66F4\u5B89\u5168\u7684\u8BA4\u8BC1\u65B9\
  \u5F0F\uFF0C\u5982OAuth\u3002\u6267\u884C\u65F6\uFF0C\u8BB0\u5F97\u4F7F\u7528HTTPS\u4EE5\
  \u9632\u4E2D\u95F4\u4EBA\u653B\u51FB\uFF0C\u4FDD\u6301\u4F20\u8F93\u52A0\u5BC6\u3002"
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
