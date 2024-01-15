---
title:                "发送 HTTP 请求"
html_title:           "C#: 发送 HTTP 请求"
simple_title:         "发送 HTTP 请求"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c-sharp/sending-an-http-request.md"
---

{{< edit_this_page >}}

## 为什么

发送HTTP请求是许多C#程序员必须掌握的技能。它使他们能够与网络资源进行交互，从而实现更多功能。

## 如何实现

以下是一个简单的示例，演示如何使用C#发送HTTP请求并获取响应：

```C#
using System;
using System.Net.Http;

class Program
{
    static async System.Threading.Tasks.Task Main(string[] args)
    {
        // 创建一个HttpClient实例
        var client = new HttpClient();

        // 发送GET请求并获取响应
        HttpResponseMessage response = await client.GetAsync("https://myapi.com/users");

        // 读取响应内容并打印
        string responseBody = await response.Content.ReadAsStringAsync();
        Console.WriteLine(responseBody);
    }
}
```

输出：

```
[{"id": 1, "name": "John"}, {"id": 2, "name": "Jane"}, {"id": 3, "name": "Bob"}]
```

## 深入了解

除了上述简单的例子，还可以使用C#发送更复杂的HTTP请求。要添加请求头、设置请求体或使用特定的HTTP方法，可以通过创建HttpRequestMessage对象来实现。此外，还可以使用HttpClient实例的SendAsync()方法发送请求，并使用HttpResponseMessage对象获取响应。

## 参考链接

- [HttpClient Class (Microsoft Docs)](https://docs.microsoft.com/en-us/dotnet/api/system.net.http.httpclient?view=netframework-4.8)
- [Sending HTTP Requests Using C# (C-SharpCorner)](https://www.c-sharpcorner.com/article/sending-http-requests-using-c-sharp/)