---
title:                "用基本身份验证发送http请求"
html_title:           "C#: 用基本身份验证发送http请求"
simple_title:         "用基本身份验证发送http请求"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c-sharp/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

什么是HTTP请求和基本认证？为什么程序员要这么做？
发送HTTP请求是指向服务器发送请求以获取特定网页或数据的过程。程序员使用基本认证来确保只有经过授权的用户可以访问特定的网页或数据。

如何进行: 
下面是一个用C＃发送HTTP请求并使用基本认证的示例。 
```
// 建立一个HttpWebRequest对象，指定目标URL
HttpWebRequest request = (HttpWebRequest)WebRequest.Create("https://example.com/data");

// 设置认证信息
request.Credentials = new NetworkCredential("用户名", "密码");

// 发送GET请求并接收响应
HttpWebResponse response = (HttpWebResponse)request.GetResponse();

// 获取响应的状态码
int statusCode = (int)response.StatusCode;

// 打印响应状态码
Console.WriteLine("状态码: {0}", statusCode);
```

探究：
基本认证是HTTP认证中最早的一种形式。它通过在HTTP请求头中包含用户名和密码来进行身份验证。它已被更安全的认证方式所取代，如OAuth和OpenID。但是，基本认证仍然被广泛使用，因为它简单、易于实现。在使用基本认证时，开发人员应注意安全性，不应将密码存储在明文中，而应使用加密存储。

查看更多：
了解更多关于HTTP请求和基本认证的信息，请参阅以下资源:
- [HTTP请求](https://www.w3.org/Protocols/rfc2616/rfc2616-sec5.html)
- [基本认证](https://www.w3.org/Protocols/rfc2617/rfc2617.html)
- [OAuth](https://oauth.net/)
- [OpenID](https://openid.net/)