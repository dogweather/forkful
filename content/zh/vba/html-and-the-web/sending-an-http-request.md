---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:01:45.459013-07:00
description: "\u5728 Visual Basic for Applications (VBA) \u4E2D\u53D1\u9001 HTTP \u8BF7\
  \u6C42\u6D89\u53CA\u901A\u8FC7 HTTP \u8BF7\u6C42\u4EE5\u7F16\u7A0B\u65B9\u5F0F\u8BBF\
  \u95EE\u7F51\u7EDC\u8D44\u6E90\u6216\u7F51\u7EDC\u670D\u52A1\u3002\u7A0B\u5E8F\u5458\
  \u8FD9\u6837\u505A\u662F\u4E3A\u4E86\u4ECE\u4ED6\u4EEC\u7684 VBA \u652F\u6301\u7684\
  \u5E94\u7528\u7A0B\u5E8F\uFF08\u5982 Excel\u3001Access \u6216\u5B9A\u5236\u7684\
  \ VBA \u89E3\u51B3\u65B9\u6848\uFF09\u4E2D\u6293\u53D6\u6570\u636E\u3001\u4E0E\u5728\
  \u7EBF API\u2026"
lastmod: 2024-02-19 22:05:06.589168
model: gpt-4-0125-preview
summary: "\u5728 Visual Basic for Applications (VBA) \u4E2D\u53D1\u9001 HTTP \u8BF7\
  \u6C42\u6D89\u53CA\u901A\u8FC7 HTTP \u8BF7\u6C42\u4EE5\u7F16\u7A0B\u65B9\u5F0F\u8BBF\
  \u95EE\u7F51\u7EDC\u8D44\u6E90\u6216\u7F51\u7EDC\u670D\u52A1\u3002\u7A0B\u5E8F\u5458\
  \u8FD9\u6837\u505A\u662F\u4E3A\u4E86\u4ECE\u4ED6\u4EEC\u7684 VBA \u652F\u6301\u7684\
  \u5E94\u7528\u7A0B\u5E8F\uFF08\u5982 Excel\u3001Access \u6216\u5B9A\u5236\u7684\
  \ VBA \u89E3\u51B3\u65B9\u6848\uFF09\u4E2D\u6293\u53D6\u6570\u636E\u3001\u4E0E\u5728\
  \u7EBF API\u2026"
title: "\u53D1\u9001HTTP\u8BF7\u6C42"
---

{{< edit_this_page >}}

## 什么 & 为什么？

在 Visual Basic for Applications (VBA) 中发送 HTTP 请求涉及通过 HTTP 请求以编程方式访问网络资源或网络服务。程序员这样做是为了从他们的 VBA 支持的应用程序（如 Excel、Access 或定制的 VBA 解决方案）中抓取数据、与在线 API 交互或以编程方式提交表单。

## 如何操作：

在 VBA 中发送 HTTP 请求的关键是使用 `Microsoft XML, v6.0` 库（或更早的版本，取决于您的系统）。首先，确保在 VBA 编辑器中通过转到 工具 > 引用 并勾选 `Microsoft XML, v6.0` 来启用此引用。

以下是如何发送简单的 HTTP GET 请求：

```vb
Dim httpRequest As Object
Set httpRequest = CreateObject("MSXML2.XMLHTTP.6.0")

With httpRequest
    .Open "GET", "https://api.example.com/data", False
    .send
    If .Status = 200 Then
        Debug.Print .responseText
    Else
        Debug.Print "Error: " & .Status & " - " & .statusText
    End If
End With
```

对于 POST 请求，我们需要向服务器发送数据（例如，JSON）：

```vb
Dim httpRequest As Object, postData As String
Set httpRequest = CreateObject("MSXML2.XMLHTTP.6.0")
postData = "{""key"":""value""}"

With httpRequest
    .Open "POST", "https://api.example.com/submit", False
    .setRequestHeader "Content-Type", "application/json"
    .send postData
    If .Status = 200 Then
        Debug.Print .responseText
    Else
        Debug.Print "Error: " & .Status & " - " & .statusText
    End If
End With
```

成功请求的示例输出可能是一个 JSON 字符串或 HTML 页面，这取决于您正在交互的 API 或网页：

```
{"data": "This is the response from the server"}
```

## 深入探索

展示的方法利用了 `MSXML2.XMLHTTP` 对象，这是 Microsoft XML Core Services (MSXML) 的一部分。它被引入是为了提供给 VBA 开发人员一种执行基于 XML 的操作的方式，并且随着时间的推移，即使在不直接处理 XML 数据的情况下，也成为 HTTP 请求的常用工具。尽管它已经有一定的年头，但对于 VBA 中的简单 Web 交互来说，它仍然是一个可靠的选择。

然而，VBA 及其 HTTP 请求机制缺乏在现代编程环境中发现的健壮性和灵活性。例如，处理异步请求或在需要高级 HTTP 功能的应用程序中工作（如 Websockets 或服务器发送的事件）超出了 VBA 的范畴。在处理更复杂的 Web 集成项目时，开发人员通常会利用外部库或工具，甚至通过网页抓取技术自动化浏览器行为，尽管这些是变通方法而不是解决方案。

像 Python 及其 `requests` 库或在 Node.js 上运行的 JavaScript 这样的语言和环境，直接提供更强大和多样化的 HTTP 请求能力，包括异步操作、更容易处理 JSON，以及对不同 Web 技术的广泛支持。深耕在 Microsoft 生态系统中的开发人员可能会考虑转向 PowerShell 或 C# 来完成需要更复杂的 Web 交互任务，利用 .NET 的广泛网络编程特性。

因此，虽然 VBA 的 HTTP 请求能力适用于简单的查询和数据获取任务，但随着项目需求向复杂和现代的 Web 景观演变，探索替代方案变得至关重要。
