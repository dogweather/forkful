---
title:                "发送http请求"
html_title:           "C#: 发送http请求"
simple_title:         "发送http请求"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c-sharp/sending-an-http-request.md"
---

{{< edit_this_page >}}

## 什么和为什么？
发送HTTP请求是一个过程，我们可以通过它从Web服务器获取数据或发送数据到Web服务器；程序员需要这样做以实现数据交互和网络通信。

## 如何实现：
C#通过HttpClient类提供了发送HTTP请求的功能。在以下示例中，我们将发送一个GET请求到指定的url，并打印出响应的数据。

```C#
using System;
using System.Net.Http;
using System.Threading.Tasks;

public class Program
{
    public static async Task Main()
    {
        HttpClient client = new HttpClient();
        
        HttpResponseMessage response = await client.GetAsync("http://example.com");

        string responseBody = await response.Content.ReadAsStringAsync();

        Console.WriteLine(responseBody);
    }
}

```
运行这段代码，你会在控制台上看到从网址"http://example.com" 返回的HTML内容。

## 深度掘进：
发送HTTP请求的方法在历史上经历了很多变化。早期版本的.NET使用`WebClient`和`HttpWebRequest`进行通信。后来，`HttpClient`在.NET 4.5中引入，并持续在.NET Core和.NET 5中得到改进和优化。

虽然`HttpClient`是主流的选择，但你还可以使用`RestSharp`、`Flurl.Http`等其他网络库。每个库都有其特定的优缺点，选择哪个库会根据你的项目需求。

由于`HttpClient`实例可以共享并重用，并且可以有效地管理网络资源，现在推荐使用`HttpClientFactory`来创建`HttpClient`实例。

## 参考链接：
1. [HttpClient类文档](https://docs.microsoft.com/zh-cn/dotnet/api/system.net.http.httpclient?view=net-5.0)
2. [HttpClientFactory使用指南](https://docs.microsoft.com/zh-cn/dotnet/architecture/microservices/implement-resilient-applications/use-httpclientfactory-to-implement-resilient-http-requests)
3. [RestSharp官方文档](https://restsharp.dev/getting-started/)
4. [Flurl.Http官方文档](https://flurl.dev/docs/flurl-http/)