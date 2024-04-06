---
date: 2024-01-20 18:00:12.593869-07:00
description: "\u5982\u4F55\uFF1A \u65E9\u5728PowerShell 1.0\u7684\u65F6\u5019\u5C31\
  \u6709\u4E86\u901A\u8FC7\u7F51\u7EDC\u53D1\u9001\u8BF7\u6C42\u7684\u80FD\u529B\uFF0C\
  \u4F46\u5B83\u4E0D\u5982\u73B0\u5728\u8FD9\u4E48\u76F4\u63A5\u548C\u5BB9\u6613\u3002\
  \u90A3\u65F6\u4F60\u53EF\u80FD\u9700\u8981\u7528\u5230 .NET \u7684 `System.Net.WebRequest`\
  \ \u7C7B\u3002\u73B0\u5728\uFF0CPowerShell \u63D0\u4F9B\u4E86`Invoke-WebRequest`\u548C\
  `Invoke-\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T22:38:47.165748-06:00'
model: gpt-4-1106-preview
summary: "\u5982\u679C\u9700\u8981\u5904\u7406RESTful API\u7684\u8BDD\uFF0C`Invoke-RestMethod`\u662F\
  \u4E2A\u4E0D\u9519\u7684\u9009\u62E9\u3002\u4E0E`Invoke-WebRequest`\u4E0D\u540C\u7684\
  \u662F\uFF0C\u5B83\u4F1A\u81EA\u52A8\u5904\u7406JSON\u548CXML\u683C\u5F0F\u7684\u54CD\
  \u5E94\u5185\u5BB9\uFF0C\u76F4\u63A5\u8FD4\u56DE\u5BF9\u8C61\uFF0C\u4E0D\u5FC5\u624B\
  \u52A8\u8F6C\u6362\u3002"
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
