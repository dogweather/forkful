---
title:                "发出 HTTP 请求"
aliases: - /zh/c-sharp/sending-an-http-request.md
date:                  2024-01-20T17:59:19.205694-07:00
model:                 gpt-4-1106-preview
simple_title:         "发出 HTTP 请求"

tag:                  "HTML and the Web"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c-sharp/sending-an-http-request.md"
---

{{< edit_this_page >}}

## What & Why?（是什么？为什么？）
HTTP请求用来与网络上的服务通信。这是因为编程时经常需要从网络上获取数据或触发远程服务。

## How to:（如何做：）
C#里发HTTP请求简单直接。先看看`HttpClient`的用法：

```c#
using System;
using System.Net.Http;
using System.Threading.Tasks;

class Program
{
    static async Task Main()
    {
        using var client = new HttpClient();
        HttpResponseMessage response = await client.GetAsync("https://api.example.com/data");
        string responseBody = await response.Content.ReadAsStringAsync();
        Console.WriteLine(responseBody);
    }
}
```

运行后输出（假设响应是JSON）：

```c#
{"name": "Example", "value": "123"}
```

## Deep Dive（深入了解）
在.NET框架出现之前，发起HTTP请求可能涉及更底层的操作，例如直接使用sockets。`HttpClient`是.NET 4.5中引入的，提供了一组易于使用的方法来发送HTTP请求。

除了`HttpClient`，还有别的选择，如`WebRequest`，以及第三方库，如`RestSharp`。但`HttpClient`是现代、异步编程的首选。

`HttpClient`用起来方便，但需要注意资源管理。一般推荐作为单例使用，不要为每个请求创建新实例。

## See Also（另请参阅）
- [HttpClient Class](https://docs.microsoft.com/en-us/dotnet/api/system.net.http.httpclient?view=net-6.0) - 官方文档，详细介绍`HttpClient`的使用。
- [Implementing HTTP call retries with exponential backoff with Polly](https://docs.microsoft.com/en-us/dotnet/architecture/microservices/implement-resilient-applications/implement-http-call-retries-exponential-backoff-polly) - 如何使用Polly实现 HTTP 调用重试与退避策略。
