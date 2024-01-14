---
title:                "PHP: 使用基本认证发送HTTP请求"
simple_title:         "使用基本认证发送HTTP请求"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/php/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

为什么：对于从事PHP编程的读者来说，发送带有基本身份验证的HTTP请求是一项重要的技能。它允许您访问需要认证的远程资源。

如何：要发送带有基本身份验证的HTTP请求，您需要使用PHP中的cURL库。以下是一个简单的示例代码，演示如何使用基本身份验证发送GET请求，并从服务器端获取响应。

```PHP
<?php
// 设置cURL选项
$ch = curl_init();
curl_setopt($ch, CURLOPT_URL, "https://example.com/api"); // 指定要发送请求的URL
curl_setopt($ch, CURLOPT_HTTPHEADER, array('Authorization: Basic ' . base64_encode($username . ':' . $password))); // 添加基本身份验证头
curl_setopt($ch, CURLOPT_RETURNTRANSFER, true); // 将响应存储在变量中而不输出到浏览器
// 发送请求
$response = curl_exec($ch);
// 处理服务器端的响应
if ($response === false) {
    // 请求失败
    echo 'cURL error: ' . curl_error($ch);
} else {
    // 请求成功
    echo $response;
}
// 关闭cURL
curl_close($ch);
?>
```

输出：

`{"status": "success", "data": {"message": "Hello, [username]!"}}`

深入了解：基本身份验证是一种HTTP协议，通过在请求头中添加编码的用户名和密码来安全地发送HTTP请求。它是一种简单而又有效的身份验证方法，被广泛用在各种Web应用程序中。您可以使用其他编码方法，如MD5或SHA-1来对用户名和密码进行加密，提高安全性。

同样重要的是要保护您的代码，以防止恶意用户访问您的用户名和密码。您可以使用PHP中的安全散列函数来对用户名和密码进行加密，以防止它们在代码中明文显示。

另外，您也可以在cURL选项中添加额外的参数来设置其他选项，例如添加超时设置或指定请求方法。

参考链接：
- [PHP官方文档 - cURL介绍](https://www.php.net/manual/en/intro.curl.php)
- [cURL官方文档](https://curl.haxx.se/docs/)
- [MDN Web文档 - HTTP基本身份验证](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication#Basic_authentication_scheme)
- [PHP安全散列函数](https://www.php.net/manual/en/function.hash.php)

## 参见