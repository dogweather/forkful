---
title:                "使用基本身份验证发送http请求"
html_title:           "PHP: 使用基本身份验证发送http请求"
simple_title:         "使用基本身份验证发送http请求"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/php/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

何处发出带基本身份验证的HTTP请求？

在网络应用开发中，我们经常需要与其他服务进行通信。有时候，这些服务会要求我们提供一些身份验证信息，以保证安全性。为了实现这一点，我们可以通过在发送HTTP请求时附上基本身份验证。

如何：

「PHP」示例代码：

```
<?php
$username = "username";
$password = "password";

$ch = curl_init();
curl_setopt($ch, CURLOPT_URL, "https://example.com/api");
curl_setopt($ch, CURLOPT_RETURNTRANSFER, true);
curl_setopt($ch, CURLOPT_USERPWD, "$username:$password");

$result = curl_exec($ch);
curl_close($ch);

echo $result;
```

实际输出：

```
{"status": "success"}
```

深入了解：

在网络通信早期，基本身份验证是为了解决HTTP协议的一些安全性问题而出现的。它采用了较简单的验证方式，使用用户名和密码来验证身份。虽然它的安全性不如其他身份验证方法，但是由于其简单易用，仍被广泛使用。

除了基本身份验证，还有其他方式来实现身份验证，如OAuth和JSON Web Tokens（JWT）。它们都提供了更加安全的方法来验证身份。但是在一些场景下，基本身份验证仍然是常用的选择。

实现一个带基本身份验证的HTTP请求需要注意一些细节。比如，用户名和密码需要通过base64编码后再附加在请求头部，以及在访问受限资源时，可能会返回401错误，需要额外处理错误情况。

相关阅读：

1. Basic Access Authentication (https://en.wikipedia.org/wiki/Basic_access_authentication)
2. OAuth (https://oauth.net/)
3. JSON Web Tokens (https://jwt.io/)