---
date: 2024-01-20 18:02:11.484220-07:00
description: ''
isCJKLanguage: true
lastmod: '2024-04-05T22:51:01.072281-06:00'
model: gpt-4-1106-preview
summary: "\u600E\u4E48\u505A \u57FA\u672C\u8BA4\u8BC1\u5728HTTP 1.0\u65F6\u88AB\u5F15\
  \u5165\uFF0C\u63D0\u4F9B\u6700\u521D\u7EA7\u7684\u5B89\u5168\u63AA\u65BD\u3002\u7528\
  \u6237\u540D\u548C\u5BC6\u7801\u672A\u52A0\u5BC6\uFF0C\u91C7\u7528Base64\u7F16\u7801\
  \uFF0C\u5BB9\u6613\u88AB\u622A\u83B7\u3002\u5B83\u7684\u66FF\u4EE3\u65B9\u6848\u5305\
  \u62ECOAuth\u548CAPI\u5BC6\u94A5\uFF0C\u901A\u5E38\u66F4\u5B89\u5168\u3002\u5F53\
  \u6267\u884CHTTP\u8BF7\u6C42\u65F6\uFF0C\u5C3D\u7BA1cURL\u662FPHP\u7684\u9996\u9009\
  \uFF0C\u4F46\u4E5F\u53EF\u4F7F\u7528file_get_contents\u548C\u6D41\u4E0A\u4E0B\u6587\
  \u3002\u5904\u7406\u54CD\u5E94\u65F6\uFF0C\u5E94\u68C0\u67E5HTTP\u72B6\u6001\u7801\
  \uFF0C\u5904\u7406cURL\u9519\u8BEF\uFF0C\u5E76\u5BF9\u6570\u636E\u8FDB\u884C\u5B89\
  \u5168\u5730\u89E3\u6790\u3002"
title: "\u4F7F\u7528\u57FA\u672C\u8BA4\u8BC1\u53D1\u9001 HTTP \u8BF7\u6C42"
weight: 45
---

## How To: 怎么做
```PHP
<?php
// 用户名和密码
$username = 'user';
$password = 'pass';

// 初始化cURL
$ch = curl_init('https://example.com/api/data');

// 配置基本认证
curl_setopt($ch, CURLOPT_HTTPAUTH, CURLAUTH_BASIC);
curl_setopt($ch, CURLOPT_USERPWD, "$username:$password");

// 设置选项获取返回值
curl_setopt($ch, CURLOPT_RETURNTRANSFER, true);

// 执行cURL会话
$response = curl_exec($ch);

// 关闭cURL资源，并释放系统资源
curl_close($ch);

// 输出响应内容
echo $response;
?>
```
Sample Output:
```
{"status":"success","data":"Confidential data here"}
```

## Deep Dive: 深入探讨
基本认证在HTTP 1.0时被引入，提供最初级的安全措施。用户名和密码未加密，采用Base64编码，容易被截获。它的替代方案包括OAuth和API密钥，通常更安全。当执行HTTP请求时，尽管cURL是PHP的首选，但也可使用file_get_contents和流上下文。处理响应时，应检查HTTP状态码，处理cURL错误，并对数据进行安全地解析。

## See Also: 参考链接
- PHP cURL 官方文档: https://www.php.net/manual/en/book.curl.php
- HTTP authentication with PHP: https://www.php.net/manual/en/features.http-auth.php
- PHP streams: https://www.php.net/manual/en/book.stream.php
- Secure your web applications: https://www.owasp.org
