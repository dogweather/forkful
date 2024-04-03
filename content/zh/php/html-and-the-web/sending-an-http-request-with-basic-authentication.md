---
date: 2024-01-20 18:02:11.484220-07:00
description: "\u5728PHP\u4E2D\u53D1\u9001\u5E26\u6709\u57FA\u672C\u8BA4\u8BC1\u7684\
  HTTP\u8BF7\u6C42\uFF0C\u5C31\u662F\u5BA2\u6237\u7AEF\u63D0\u4F9B\u7528\u6237\u540D\
  \u548C\u5BC6\u7801\u4EE5\u83B7\u53D6\u8D44\u6E90\u3002\u8FD9\u79CD\u65B9\u6CD5\u5E38\
  \u7528\u4E8E\u6570\u636E\u7684\u7B80\u5355\u4FDD\u62A4\u3001API\u7684\u4F7F\u7528\
  \uFF0C\u6216\u8005\u9700\u8981\u9A8C\u8BC1\u6743\u9650\u7684\u573A\u666F\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:47.863258-06:00'
model: gpt-4-1106-preview
summary: "\u5728PHP\u4E2D\u53D1\u9001\u5E26\u6709\u57FA\u672C\u8BA4\u8BC1\u7684HTTP\u8BF7\
  \u6C42\uFF0C\u5C31\u662F\u5BA2\u6237\u7AEF\u63D0\u4F9B\u7528\u6237\u540D\u548C\u5BC6\
  \u7801\u4EE5\u83B7\u53D6\u8D44\u6E90\u3002\u8FD9\u79CD\u65B9\u6CD5\u5E38\u7528\u4E8E\
  \u6570\u636E\u7684\u7B80\u5355\u4FDD\u62A4\u3001API\u7684\u4F7F\u7528\uFF0C\u6216\
  \u8005\u9700\u8981\u9A8C\u8BC1\u6743\u9650\u7684\u573A\u666F\u3002."
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
