---
date: 2024-01-20 17:59:19.205694-07:00
description: "HTTP\u8BF7\u6C42\u7528\u6765\u4E0E\u7F51\u7EDC\u4E0A\u7684\u670D\u52A1\
  \u901A\u4FE1\u3002\u8FD9\u662F\u56E0\u4E3A\u7F16\u7A0B\u65F6\u7ECF\u5E38\u9700\u8981\
  \u4ECE\u7F51\u7EDC\u4E0A\u83B7\u53D6\u6570\u636E\u6216\u89E6\u53D1\u8FDC\u7A0B\u670D\
  \u52A1\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:47.763951-06:00'
model: gpt-4-1106-preview
summary: "HTTP\u8BF7\u6C42\u7528\u6765\u4E0E\u7F51\u7EDC\u4E0A\u7684\u670D\u52A1\u901A\
  \u4FE1\u3002\u8FD9\u662F\u56E0\u4E3A\u7F16\u7A0B\u65F6\u7ECF\u5E38\u9700\u8981\u4ECE\
  \u7F51\u7EDC\u4E0A\u83B7\u53D6\u6570\u636E\u6216\u89E6\u53D1\u8FDC\u7A0B\u670D\u52A1\
  \u3002"
title: "\u53D1\u51FA HTTP \u8BF7\u6C42"
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
