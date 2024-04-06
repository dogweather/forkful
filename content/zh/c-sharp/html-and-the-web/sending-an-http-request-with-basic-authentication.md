---
date: 2024-01-20 18:01:14.310810-07:00
description: "How to: (\u5982\u4F55\u64CD\u4F5C) \u57FA\u672C\u8BA4\u8BC1\u7531HTTP\u534F\
  \u8BAE\u5B9A\u4E49\uFF0C\u6700\u65E9\u57281996\u5E74\u7684RFC 1945\u4E2D\u63D0\u51FA\
  \u3002\u5B83\u4E0D\u662F\u6700\u5B89\u5168\u7684\u8BA4\u8BC1\u65B9\u5F0F\u2014\u2014\
  \u6BD4\u5982\uFF0C\u82E5\u6CA1\u6709TLS/SSL\uFF0C\u51ED\u636E\u53EF\u88AB\u622A\u83B7\
  \u3002\u66F4\u5B89\u5168\u7684\u66FF\u4EE3\u65B9\u6CD5\u5305\u62ECOAuth2.0\u548C\
  JWT (JSON Web\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T22:51:00.975882-06:00'
model: gpt-4-1106-preview
summary: "(\u5982\u4F55\u64CD\u4F5C) \u57FA\u672C\u8BA4\u8BC1\u7531HTTP\u534F\u8BAE\
  \u5B9A\u4E49\uFF0C\u6700\u65E9\u57281996\u5E74\u7684RFC 1945\u4E2D\u63D0\u51FA\u3002\
  \u5B83\u4E0D\u662F\u6700\u5B89\u5168\u7684\u8BA4\u8BC1\u65B9\u5F0F\u2014\u2014\u6BD4\
  \u5982\uFF0C\u82E5\u6CA1\u6709TLS/SSL\uFF0C\u51ED\u636E\u53EF\u88AB\u622A\u83B7\u3002\
  \u66F4\u5B89\u5168\u7684\u66FF\u4EE3\u65B9\u6CD5\u5305\u62ECOAuth2.0\u548CJWT (JSON\
  \ Web Tokens)\u3002\u5728C#\u4E2D\uFF0C\u53D1\u9001\u5E26\u57FA\u672C\u8BA4\u8BC1\
  \u7684HTTP\u8BF7\u6C42\uFF0C\u91CD\u70B9\u5728\u4E8E\u751F\u6210\u5408\u9002\u7684\
  `Authorization`\u5934\u90E8\uFF0C\u8BE5\u5934\u90E8\u542B\u6709Base64\u7F16\u7801\
  \u7684\u7528\u6237\u540D\u548C\u5BC6\u7801\u3002\u8FD8\u53EF\u4EE5\u7528`HttpClientHandler`\u6765\
  \u8BBE\u7F6E\u51ED\u636E\uFF0CC# 5.0\u53CA\u4EE5\u4E0A\u7248\u672C\u652F\u6301`async`\u548C\
  `await`\u7528\u4E8E\u66F4\u9AD8\u6548\u7684\u5F02\u6B65\u64CD\u4F5C\u3002"
title: "\u4F7F\u7528\u57FA\u672C\u8BA4\u8BC1\u53D1\u9001 HTTP \u8BF7\u6C42"
weight: 45
---

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
