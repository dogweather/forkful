---
date: 2024-01-20 18:00:12.593869-07:00
description: "\u5982\u4F55\uFF1A ."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:48.009888-06:00'
model: gpt-4-1106-preview
summary: .
title: "\u53D1\u51FA HTTP \u8BF7\u6C42"
weight: 44
---

## 如何：
```PowerShell
# 发送GET请求
$response = Invoke-WebRequest -Uri 'http://example.com/api/data' -Method 'GET'
# 输出响应内容
$response.Content

# 发送POST请求，带有数据
$body = @{ username='user'; password='pass' } | ConvertTo-Json
$response = Invoke-WebRequest -Uri 'http://example.com/api/login' -Method 'POST' -Body $body -ContentType 'application/json'
# 输出响应状态码
$response.StatusCode
```

## 深入探索
早在PowerShell 1.0的时候就有了通过网络发送请求的能力，但它不如现在这么直接和容易。那时你可能需要用到 .NET 的 `System.Net.WebRequest` 类。现在，PowerShell 提供了`Invoke-WebRequest`和`Invoke-RestMethod`这两个高级命令，能更方便地处理HTTP请求。

如果需要处理RESTful API的话，`Invoke-RestMethod`是个不错的选择。与`Invoke-WebRequest`不同的是，它会自动处理JSON和XML格式的响应内容，直接返回对象，不必手动转换。

在实现上，这些命令层面调用的是.NET Framework的HttpClient类，继承了它的高效和强大的功能。尽管有些人会倾向于使用`curl`或者`wget`这样的传统命令行工具，但在PowerShell中，`Invoke-WebRequest`和`Invoke-RestMethod`对于许多HTTP请求来说都是一种更方便、原生的解决方案。

## 参考链接
- [Invoke-WebRequest 官方文档](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/invoke-webrequest)
- [Invoke-RestMethod 官方文档](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/invoke-restmethod)
- [关于 HTTP 请求的更多信息](https://developer.mozilla.org/en-US/docs/Web/HTTP/Methods)
