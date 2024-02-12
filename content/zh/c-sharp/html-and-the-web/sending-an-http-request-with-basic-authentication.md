---
title:                "使用基本认证发送 HTTP 请求"
aliases:
- /zh/c-sharp/sending-an-http-request-with-basic-authentication.md
date:                  2024-01-20T18:01:14.310810-07:00
model:                 gpt-4-1106-preview
simple_title:         "使用基本认证发送 HTTP 请求"

tag:                  "HTML and the Web"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c-sharp/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## What & Why? (什么 & 为什么?)
发送带基本认证的HTTP请求，指使用用户名和密码获取网络资源。程序员这样做是为了安全访问限制区域。

## How to: (如何操作)
简单代码示例：

```C#
using System;
using System.Net.Http;
using System.Text;
using System.Threading.Tasks;

class BasicAuthExample
{
    static async Task Main(string[] args)
    {
        var client = new HttpClient();
        var credentials = Convert.ToBase64String(Encoding.ASCII.GetBytes("username:password"));
        client.DefaultRequestHeaders.Authorization = new System.Net.Http.Headers.AuthenticationHeaderValue("Basic", credentials);

        try
        {
            HttpResponseMessage response = await client.GetAsync("http://example.com/protected");
            response.EnsureSuccessStatusCode();
            string responseBody = await response.Content.ReadAsStringAsync();
            Console.WriteLine(responseBody);
        }
        catch(HttpRequestException e)
        {
            Console.WriteLine("\nException Caught!");	
            Console.WriteLine("Message :{0} ",e.Message);
        }
    }
}
```

输出样例：

```
<!DOCTYPE html>
<html>
<body>
    <h1>Welcome to the protected area</h1>
</body>
</html>
```

## Deep Dive (深入了解)
基本认证由HTTP协议定义，最早在1996年的RFC 1945中提出。它不是最安全的认证方式——比如，若没有TLS/SSL，凭据可被截获。更安全的替代方法包括OAuth2.0和JWT (JSON Web Tokens)。在C#中，发送带基本认证的HTTP请求，重点在于生成合适的`Authorization`头部，该头部含有Base64编码的用户名和密码。还可以用`HttpClientHandler`来设置凭据，C# 5.0及以上版本支持`async`和`await`用于更高效的异步操作。

## See Also (另请参阅)
- HTTP基本认证规范: [RFC7617](https://tools.ietf.org/html/rfc7617)
- C# `HttpClient` 类: [MSDN Documentation](https://docs.microsoft.com/en-us/dotnet/api/system.net.http.httpclient)
- Base64编码: [Base64 on Wikipedia](https://en.wikipedia.org/wiki/Base64)
- 关于OAuth 2.0: [OAuth 2.0 Authorization Framework](https://oauth.net/2/)
- 关于JWT (JSON Web Tokens): [Introduction to JSON Web Tokens](https://jwt.io/introduction/)
