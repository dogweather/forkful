---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:04:00.725036-07:00
description: "\u5728 Visual Basic for Applications (VBA) \u4E2D\u4F7F\u7528\u57FA\u672C\
  \u8BA4\u8BC1\u53D1\u9001 HTTP \u8BF7\u6C42\u6D89\u53CA\u5230\u8BBF\u95EE\u53D7\u7528\
  \u6237\u540D\u548C\u5BC6\u7801\u51ED\u636E\u4FDD\u62A4\u7684 web \u8D44\u6E90\u3002\
  \u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\u662F\u4E3A\u4E86\u4E0E\u5176 VBA \u9A71\u52A8\
  \u7684\u5E94\u7528\u7A0B\u5E8F\u4E2D\u7684\u5B89\u5168 API \u6216\u7F51\u7EDC\u670D\
  \u52A1\u8FDB\u884C\u4EA4\u4E92\uFF0C\u4F8B\u5982\u5728 Excel \u6216 Access\u2026"
lastmod: '2024-03-13T22:44:47.572450-06:00'
model: gpt-4-0125-preview
summary: "\u5728 Visual Basic for Applications (VBA) \u4E2D\u4F7F\u7528\u57FA\u672C\
  \u8BA4\u8BC1\u53D1\u9001 HTTP \u8BF7\u6C42\u6D89\u53CA\u5230\u8BBF\u95EE\u53D7\u7528\
  \u6237\u540D\u548C\u5BC6\u7801\u51ED\u636E\u4FDD\u62A4\u7684 web \u8D44\u6E90\u3002\
  \u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\u662F\u4E3A\u4E86\u4E0E\u5176 VBA \u9A71\u52A8\
  \u7684\u5E94\u7528\u7A0B\u5E8F\u4E2D\u7684\u5B89\u5168 API \u6216\u7F51\u7EDC\u670D\
  \u52A1\u8FDB\u884C\u4EA4\u4E92\uFF0C\u4F8B\u5982\u5728 Excel \u6216 Access \u4E2D\
  \u81EA\u52A8\u5316\u6267\u884C\u4EFB\u52A1\uFF0C\u8FD9\u4E9B\u4EFB\u52A1\u6765\u81EA\
  \u4E8E\u53D7\u4FDD\u62A4\u7AEF\u70B9\u7684\u6570\u636E\u3002."
title: "\u4F7F\u7528\u57FA\u672C\u8BA4\u8BC1\u53D1\u9001HTTP\u8BF7\u6C42"
weight: 45
---

## 如何实现:
在 VBA 中，你可以使用 `Microsoft XML, v6.0`（MSXML2）库发送带有基本认证的 HTTP 请求。这涉及到设置请求的 “Authorization” 头部，以包含以 base64 编码格式的凭证。以下是逐步指南：

1. **参考 MSXML2**：首先，确保你的 VBA 项目引用了 `Microsoft XML, v6.0` 库。在 VBA 编辑器中，转到工具 > 引用，然后勾选 `Microsoft XML, v6.0`。

2. **创建并发送 HTTP 请求**：使用以下 VBA 代码片段作为指南。用你的实际凭据替换 `"your_username"` 和 `"your_password"`，并根据需要调整 URL。

    ```vb
    Dim XMLHttp As Object
    Set XMLHttp = CreateObject("MSXML2.XMLHTTP")
    Dim url As String
    url = "http://example.com/api/resource" ' 用实际的 URL 替换
    Dim base64Credentials As String
    base64Credentials = EncodeBase64("your_username:your_password")
    
    XMLHttp.Open "GET", url, False
    XMLHttp.setRequestHeader "Authorization", "Basic " & base64Credentials
    XMLHttp.send
    
    Debug.Print XMLHttp.responseText ' 将响应输出到即时窗口
    ```

3. **以 base64 编码凭证**：VBA 没有内置的 base64 编码函数，但你可以使用此自定义 `EncodeBase64` 函数：

    ```vb
    Function EncodeBase64(text As String) As String
        Dim arrData() As Byte
        arrData = StrConv(text, vbFromUnicode)
        
        Dim objXML As MSXML2.DOMDocument60
        Dim objNode As MSXML2.IXMLDOMElement
        
        Set objXML = New MSXML2.DOMDocument60
        Set objNode = objXML.createElement("b64")
        
        objNode.dataType = "bin.base64"
        objNode.nodeTypedValue = arrData
        EncodeBase64 = objNode.Text
    End Function
    ```
    
这将向 `http://example.com/api/resource` 发送一个带有指定基本认证凭证的 GET 请求，并打印响应。

## 深入解析
这里使用的方法，虽然对于简单的用例是有效的，但它依赖于基本认证方案，该方案将凭证以容易被解码的格式（base64 编码并不是加密）发送。由于其易受攻击性，尤其是在非 HTTPS 上下文中，不建议在没有 SSL/TLS 这样的额外安全层的情况下通过互联网传输敏感数据使用基本认证。

历史上，基本认证是为控制访问 web 资源开发的最早方法之一。今天，对于新应用程序，一般更倾向于使用更安全和更灵活的认证标准，如 OAuth 2.0。鉴于 VBA 的限制和实现更高级认证方法所需的外部依赖，开发人员通常在内部或对安全要求不太高的环境中使用 VBA，或将其作为快速原型设计的跳板。

在使用 VBA 进行 HTTP 请求时，记住 MSXML 库的每个版本可能支持不同的功能和安全标准。始终使用与你的应用程序兼容的最新版本，以确保更好的安全性和性能。此外，当选择 VBA 用于需要安全 HTTP 通信的新项目时，还应考虑到环境限制和潜在的废弃特性。对于类似任务，其他编程环境或语言可能提供更健壮、更安全、更可维护的解决方案。
