---
title:                "C#: 使用基本认证发送http请求。"
simple_title:         "使用基本认证发送http请求。"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c-sharp/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 为什么

在编写网络应用程序时，您可能需要向远程服务器发送HTTP请求。使用基本身份验证可以提供对您的应用程序的安全保护，以防止未经授权的访问。通过发送具有基本身份验证的HTTP请求，您可以确保只有经过身份验证的用户才能访问您的应用程序。

## 如何进行

对于C＃开发人员，发送带有基本身份验证的HTTP请求非常简单。您可以通过以下代码示例来了解如何实现：

```C#
// 引用必要的命名空间
using System;
using System.Net.Http;

// 准备HTTP请求的URL和基本身份验证凭据
string url = "https://example.com/api";
string username = "myUsername";
string password = "myPassword";

// 创建 HttpClient 对象
HttpClient client = new HttpClient();

// 创建基本身份验证凭据
var credentials = new System.Net.NetworkCredential(username, password);

// 将凭据添加到 HttpClient 客户端的默认凭据
client.DefaultRequestHeaders.Authorization = new System.Net.Http.Headers.AuthenticationHeaderValue("Basic", Convert.ToBase64String(credentials.GetBytes()));

// 发送 HTTP GET 请求
HttpResponseMessage response = await client.GetAsync(url);

// 从响应中获取内容
string content = await response.Content.ReadAsStringAsync();

// 打印响应内容
Console.WriteLine(content);
```

上面的代码示例将向`https://example.com/api`发送带有基本身份验证凭据的HTTP GET请求，并将响应的内容打印到控制台。您可以根据您的应用程序需求修改代码来发送不同类型的HTTP请求。

## 深入了解

基本身份验证是一种最简单的身份验证方法，它涉及将用户名和密码作为HTTP请求的一部分发送到服务器。服务器接收到请求后，将检查凭据是否匹配，并且只有在凭据正确的情况下才允许访问。

通常，HTTP请求的凭据是在请求头中添加的，并以`Authorization`字段的形式发送。该字段的值由`Basic`加上凭据的Base64编码组成，例如`Basic bXlVc2VybmFtZTpteVBhc3N3b3Jk`。

注意，基本身份验证并不是最安全的身份验证方法，因为凭据将以明文形式发送，易于被窃取。因此，对于安全性要求更高的应用程序，您可能需要考虑使用其他身份验证方法。

## 参见

- [HTTP Authentication: Basic and Digest Access Authentication](https://developer.mozilla.org/zh-CN/docs/Web/HTTP/Authentication)
- [Working with HttpClient](https://docs.microsoft.com/zh-cn/dotnet/api/system.net.http.httpclient?view=net-5.0)
- [Base64 Encoding](https://docs.microsoft.com/zh-cn/dotnet/api/system.convert.tobase64string?view=net-5.0)

感谢阅读本篇博客，希望这篇文章能帮助您更好地理解如何发送带有基本身份验证的HTTP请求。如果您有任何疑问或建议，请在评论中与我分享。谢谢！