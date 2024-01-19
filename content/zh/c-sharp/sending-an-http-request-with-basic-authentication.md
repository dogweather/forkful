---
title:                "使用基本认证发送http请求"
html_title:           "Bash: 使用基本认证发送http请求"
simple_title:         "使用基本认证发送http请求"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c-sharp/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 何为何？ - 什么是使用基本认证发送HTTP请求，程序员为什么要做？

使用基本认证发送HTTP请求是一个过程，其中，我们使用用户名和密码创建请求头，以进行身份验证。程序员通常会做这件事情，以便从需要身份验证的服务器上获取资源。

## 如何做 - 使用C#发送带有基本认证的HTTP请求

以下是一个简单的示例，显示如何使用HttpClient发送带有基础认证（用户名和密码）的HTTP GET请求：

```C#
using System;
using System.Net.Http;
using System.Text;
using System.Threading.Tasks;

public class BasicAuthExample{
    static async Task Main()
    {
        var client = new HttpClient();

        var byteArray = Encoding.ASCII.GetBytes("username:password");
        client.DefaultRequestHeaders.Authorization = new System.Net.Http.Headers.AuthenticationHeaderValue("Basic", Convert.ToBase64String(byteArray));

        var response = await client.GetAsync("http://example.com/resource");
        var data = await response.Content.ReadAsStringAsync();

        Console.WriteLine(data);
    }
}
```

这个示例会向"http://example.com/resource"发送一个HTTP GET请求，并在控制台上打印返回的数据。

## 深入剖析 - HTTP Basic认证的历史，替代方案，以及实现细节

HTTP Basic认证是一个老牌的身份验证方案，其源于HTTP/1.0的时代，它现在依然被广泛支持，但有其安全性问题。

作为替代方案，我们有Digest认证、Bearer认证（常用于OAuth）、或者使用一个基于token的身份验证方法。

在实现上，Basic认证是将用户名和密码以":"组合，并Encode成base64字符串，然后将这字符串作为Authorization头的一部分发送。这是一个非常直接的方式，但注意，如果不使用HTTPS，它容易被窃取。

## 参考资料 - 了解更多关于HTTP认证的资料

1. [理解HTTP Basic认证](https://developer.mozilla.org/zh-CN/docs/Web/HTTP/Headers/Authorization)
2. [C#中HttpClient类的使用](https://docs.microsoft.com/zh-cn/dotnet/api/system.net.http.httpclient)
3. [更多关于HTTPs的信息](https://developer.mozilla.org/zh-CN/docs/Web/HTTP/Overview)