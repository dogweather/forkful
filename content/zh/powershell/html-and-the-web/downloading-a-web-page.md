---
date: 2024-01-20 17:44:51.579440-07:00
description: "How to: (\u5982\u4F55\u64CD\u4F5C\uFF1A) PowerShell \u91CC\uFF0C\u7528\
  \ `Invoke-WebRequest` \u53EF\u4EE5\u4E0B\u8F7D\u7F51\u9875\u3002\u7B80\u5355\u4F8B\
  \u5B50\uFF1A."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:48.012119-06:00'
model: gpt-4-1106-preview
summary: "PowerShell \u91CC\uFF0C\u7528 `Invoke-WebRequest` \u53EF\u4EE5\u4E0B\u8F7D\
  \u7F51\u9875\u3002\u7B80\u5355\u4F8B\u5B50\uFF1A."
title: "\u4E0B\u8F7D\u7F51\u9875"
weight: 42
---

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
