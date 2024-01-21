---
title:                "下载网页"
date:                  2024-01-20T17:44:51.579440-07:00
model:                 gpt-4-1106-preview
simple_title:         "下载网页"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/powershell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## What & Why? (是什么以及为什么？)
下载网页就是将网页内容从服务器保存到本地计算机。程序员这样做可能是为了数据分析、备份或离线阅读。

## How to: (如何操作：)
PowerShell 里，用 `Invoke-WebRequest` 可以下载网页。简单例子：

```PowerShell
$url = "http://example.com"
$output = "C:\myfolder\example.html"
Invoke-WebRequest -Uri $url -OutFile $output
```

下载后，`example.html` 将包含网页的全部内容。

## Deep Dive (深入了解)
`Invoke-WebRequest` 是 PowerShell 3.0 引入的，用来发送 HTTP 请求。历史上，我们可能用 `WebClient` 或 `HttpWebRequest`。不过，`Invoke-WebRequest` 更简洁。

它不只下载页面，还能抓取链接、表单等。还支持 HTTP 动词如 GET、POST。

比如，登录网站：

```PowerShell
$loginInfo = @{
    username = 'yourname'
    password = 'yourpassword'
}
Invoke-WebRequest -Uri 'http://example.com/login' -Method Post -Body $loginInfo
```

实现上，它依赖 .NET Framework 或 .NET Core 的网络功能。

## See Also (另请参阅)
- [Invoke-WebRequest 官方文档](https://docs.microsoft.com/en-us/powershell/module/Microsoft.PowerShell.Utility/Invoke-WebRequest)
- [关于 .NET 网络请求的更多信息](https://docs.microsoft.com/en-us/dotnet/framework/network-programming/)