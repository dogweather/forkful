---
title:                "发送http请求"
html_title:           "C#: 发送http请求"
simple_title:         "发送http请求"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/powershell/sending-an-http-request.md"
---

{{< edit_this_page >}}

## 什么 & 为什么?

HTTP请求是一种让应用程序（例如，浏览器）与服务器通信的方式。程序员使用HTTP请求来从网络资源获取数据、提交数据、和触发服务器的某些动作。

## 如何实现:

要在 PowerShell 中发送 HTTP 请求，可以使用 `Invoke-WebRequest` 或 `Invoke-RestMethod` cmdlet。它们都支持 GET、POST、PUT、DELETE 等 HTTP 方法。这是一个基本例子：

```PowerShell
$response = Invoke-WebRequest -Uri 'https://api.github.com/users/octocat'
$response.StatusCode
```
`$response.StatusCode` 将显示 HTTP 状态代码，例如，200 表示请求成功。

例如：

```PowerShell
200
```

## 深入探讨

PowerShell 中的 HTTP 请求在早期版本中并不存在。在 PowerShell 3.0 版本中，Microsoft introduces introduce `Invoke-WebRequest` and `Invoke-RestMethod` cmdlets. 发送 HTTP 请求在许多场景下都非常有用，例如云端服务、RESTful API、和网页抓取等。

在 PowerShell 外，也有其他方式发送 HTTP 请求，例如使用 cURL 或 wget。这些工具和 PowerShell 的 cmdlets 功能类似，但其他工具的优势在于它们支持更多的协议和选项。

至于 PowerShell 的 HTTP 请求实现，主要是基于 .NET Framework 的 `System.Net.HttpWebRequest` 类。这个类支持各种 HTTP 版本，并且支持同步和异步操作。

## 另请参阅

[PowerShell 官方文档](https://docs.microsoft.com/en-us/powershell/scripting/overview?view=powershell-7.1): 这是 PowerShell 的官方文档，详细解说了 PowerShell 的使用方法，包括如何发送 HTTP 请求。

[RestSharp 库](https://restsharp.dev/getting-started/installation.html): 是一个功能强大的、易于使用的 .NET HTTP 客户端库。它可以用于替代 PowerShell 的 HTTP 请求功能。

[.NET HttpWebRequest 类](https://docs.microsoft.com/zh-cn/dotnet/api/system.net.httpwebrequest?view=net-5.0): 你可以参照这个文档了解如何使用 .NET Framework 的 `HttpWebRequest` 类发送 HTTP 请求。