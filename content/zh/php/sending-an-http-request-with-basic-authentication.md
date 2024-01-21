---
title:                "使用基本认证发送 HTTP 请求"
date:                  2024-01-20T18:02:11.484220-07:00
model:                 gpt-4-1106-preview
simple_title:         "使用基本认证发送 HTTP 请求"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/php/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## What & Why? 什么与为什么?
在PHP中发送带有基本认证的HTTP请求，就是客户端提供用户名和密码以获取资源。这种方法常用于数据的简单保护、API的使用，或者需要验证权限的场景。

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