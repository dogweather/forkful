---
title:                "用基本认证发送http请求"
html_title:           "PHP: 用基本认证发送http请求"
simple_title:         "用基本认证发送http请求"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/php/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 为什么
在现代的网络开发工作中，发送HTTP请求是非常常见的。使用基本身份验证能够确保你的请求是安全和私密的，以保护用户的个人信息或敏感数据。 

## 如何
在PHP中，我们可以通过使用‘ curl_init’ 和 ‘curl_setopt’函数来发送带有基本身份验证的HTTP请求。首先，我们需要使用‘curl_init’函数来初始化一个curl会话，然后使用‘curl_setopt’将请求的URL和身份验证选项设置为CURLOPT_URL和CURLOPT_USERPWD。接下来，我们可以使用‘curl_exec’函数来执行请求，并通过‘curl_getinfo’函数获取请求的输出结果。

```PHP
$ch = curl_init(); // 初始化curl会话
curl_setopt($ch, CURLOPT_URL, "https://www.example.com"); // 设置请求的URL
curl_setopt($ch, CURLOPT_USERPWD, "username:password"); // 设置基本身份验证选项
$result = curl_exec($ch); // 执行请求
$info = curl_getinfo($ch); // 获取请求的输出结果
curl_close($ch); // 关闭curl会话
echo $result; // 输出请求的结果
```

在上面的代码示例中，我们使用‘username’和‘password’作为示例的基本身份验证凭证。根据实际情况，您需要替换为您自己的用户名和密码。

## 深入探讨
基本身份验证是一种使用用户名和密码来验证请求的方法。当您向服务器发送HTTP请求时，您提供的用户名和密码将被加密并作为HTTP头的一部分加入到请求中。服务器接收到请求后，将对提供的用户名和密码进行验证，如果验证成功，则允许继续访问所请求的资源。

## 参考资料
- [PHP官方文档 - curl_init()](https://www.php.net/manual/en/function.curl-init.php)
- [PHP官方文档 - curl_setopt()](https://www.php.net/manual/en/function.curl-setopt.php)
- [PHP官方文档 - curl_exec()](https://www.php.net/manual/en/function.curl-exec.php)
- [PHP官方文档 - curl_getinfo()](https://www.php.net/manual/en/function.curl-getinfo.php)