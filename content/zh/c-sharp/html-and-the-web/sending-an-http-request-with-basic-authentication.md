---
date: 2024-01-20 18:01:14.310810-07:00
description: "\u53D1\u9001\u5E26\u57FA\u672C\u8BA4\u8BC1\u7684HTTP\u8BF7\u6C42\uFF0C\
  \u6307\u4F7F\u7528\u7528\u6237\u540D\u548C\u5BC6\u7801\u83B7\u53D6\u7F51\u7EDC\u8D44\
  \u6E90\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\u662F\u4E3A\u4E86\u5B89\u5168\u8BBF\
  \u95EE\u9650\u5236\u533A\u57DF\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:47.767225-06:00'
model: gpt-4-1106-preview
summary: "\u53D1\u9001\u5E26\u57FA\u672C\u8BA4\u8BC1\u7684HTTP\u8BF7\u6C42\uFF0C\u6307\
  \u4F7F\u7528\u7528\u6237\u540D\u548C\u5BC6\u7801\u83B7\u53D6\u7F51\u7EDC\u8D44\u6E90\
  \u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\u662F\u4E3A\u4E86\u5B89\u5168\u8BBF\u95EE\
  \u9650\u5236\u533A\u57DF\u3002"
title: "\u4F7F\u7528\u57FA\u672C\u8BA4\u8BC1\u53D1\u9001 HTTP \u8BF7\u6C42"
weight: 45
---

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
