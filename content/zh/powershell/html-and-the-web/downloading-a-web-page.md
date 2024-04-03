---
date: 2024-01-20 17:44:51.579440-07:00
description: "\u4E0B\u8F7D\u7F51\u9875\u5C31\u662F\u5C06\u7F51\u9875\u5185\u5BB9\u4ECE\
  \u670D\u52A1\u5668\u4FDD\u5B58\u5230\u672C\u5730\u8BA1\u7B97\u673A\u3002\u7A0B\u5E8F\
  \u5458\u8FD9\u6837\u505A\u53EF\u80FD\u662F\u4E3A\u4E86\u6570\u636E\u5206\u6790\u3001\
  \u5907\u4EFD\u6216\u79BB\u7EBF\u9605\u8BFB\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:48.012119-06:00'
model: gpt-4-1106-preview
summary: "\u4E0B\u8F7D\u7F51\u9875\u5C31\u662F\u5C06\u7F51\u9875\u5185\u5BB9\u4ECE\
  \u670D\u52A1\u5668\u4FDD\u5B58\u5230\u672C\u5730\u8BA1\u7B97\u673A\u3002\u7A0B\u5E8F\
  \u5458\u8FD9\u6837\u505A\u53EF\u80FD\u662F\u4E3A\u4E86\u6570\u636E\u5206\u6790\u3001\
  \u5907\u4EFD\u6216\u79BB\u7EBF\u9605\u8BFB\u3002."
title: "\u4E0B\u8F7D\u7F51\u9875"
weight: 42
---

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
