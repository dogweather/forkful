---
title:                "下载网页"
html_title:           "Arduino: 下载网页"
simple_title:         "下载网页"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c-sharp/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## 什么以及为什么？  

下载网页就是将互联网上的一个页面保存到本地。程序员这样做主要是为了保存网页内容，或在离线时进行浏览和分析。

## 如何做：

```C#
using System;
using System.Net;
using System.IO;

class Program
{
    static void Main()
    {
        WebClient client = new WebClient();
        string reply = client.DownloadString("https://www.google.com");

        Console.WriteLine(reply);
    }
}
```
上述代码将下载https://www.google.com的html内容,并在终端中显示。

## 深入探讨：

历史背景：早在20世纪90年代，人们就开始使用HTTP请求来下载网页，那时的网络环境比现在复杂得多。

替代方案：除了使用C# 中的 WebClient，还可以使用HttpClient类下载网页。 HttpClient提供了更现代、功能强大的API。

实现详情：WebClient实际上是在一定的抽象级别下利用了HTTP或HTTPS协议（取决于URL）进行工作。 当你调用 DownloadString， WebClient 会发出GET请求，服务器则响应该请求并返回查询的网页内容。

## 另请参阅：

关于WebClient和HttpClient的详细信息和比较，请参阅微软官方文档：
- WebClient: https://docs.microsoft.com/en-us/dotnet/api/system.net.webclient?view=net-5.0
- HttpClient: https://docs.microsoft.com/en-us/dotnet/api/system.net.http.httpclient?view=net-5.0

如需更深入了解下载网页的历史，请参阅：
- HTTP历史: https://www.w3.org/Protocols/History.html