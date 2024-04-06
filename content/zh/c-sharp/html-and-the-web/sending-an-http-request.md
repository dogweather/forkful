---
date: 2024-01-20 17:59:19.205694-07:00
description: "How to:\uFF08\u5982\u4F55\u505A\uFF1A\uFF09 C#\u91CC\u53D1HTTP\u8BF7\
  \u6C42\u7B80\u5355\u76F4\u63A5\u3002\u5148\u770B\u770B`HttpClient`\u7684\u7528\u6CD5\
  \uFF1A."
isCJKLanguage: true
lastmod: '2024-04-05T22:38:46.927001-06:00'
model: gpt-4-1106-preview
summary: "\uFF08\u5982\u4F55\u505A\uFF1A\uFF09 C#\u91CC\u53D1HTTP\u8BF7\u6C42\u7B80\
  \u5355\u76F4\u63A5\u3002\u5148\u770B\u770B`HttpClient`\u7684\u7528\u6CD5\uFF1A."
title: "\u53D1\u51FA HTTP \u8BF7\u6C42"
weight: 44
---

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
