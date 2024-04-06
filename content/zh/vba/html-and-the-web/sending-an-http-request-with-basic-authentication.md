---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:04:00.725036-07:00
description: "\u5982\u4F55\u5B9E\u73B0: \u5728 VBA \u4E2D\uFF0C\u4F60\u53EF\u4EE5\u4F7F\
  \u7528 `Microsoft XML, v6.0`\uFF08MSXML2\uFF09\u5E93\u53D1\u9001\u5E26\u6709\u57FA\
  \u672C\u8BA4\u8BC1\u7684 HTTP \u8BF7\u6C42\u3002\u8FD9\u6D89\u53CA\u5230\u8BBE\u7F6E\
  \u8BF7\u6C42\u7684 \u201CAuthorization\u201D \u5934\u90E8\uFF0C\u4EE5\u5305\u542B\
  \u4EE5 base64 \u7F16\u7801\u683C\u5F0F\u7684\u51ED\u8BC1\u3002\u4EE5\u4E0B\u662F\
  \u9010\u6B65\u6307\u5357\uFF1A 1. **\u53C2\u8003 MSXML2**\uFF1A\u9996\u5148\uFF0C\
  \u786E\u4FDD\u4F60\u7684 VBA\u2026"
lastmod: '2024-04-05T21:53:47.892295-06:00'
model: gpt-4-0125-preview
summary: "\u5728 VBA \u4E2D\uFF0C\u4F60\u53EF\u4EE5\u4F7F\u7528 `Microsoft XML, v6.0`\uFF08\
  MSXML2\uFF09\u5E93\u53D1\u9001\u5E26\u6709\u57FA\u672C\u8BA4\u8BC1\u7684 HTTP \u8BF7\
  \u6C42\u3002\u8FD9\u6D89\u53CA\u5230\u8BBE\u7F6E\u8BF7\u6C42\u7684 \u201CAuthorization\u201D\
  \ \u5934\u90E8\uFF0C\u4EE5\u5305\u542B\u4EE5 base64 \u7F16\u7801\u683C\u5F0F\u7684\
  \u51ED\u8BC1\u3002\u4EE5\u4E0B\u662F\u9010\u6B65\u6307\u5357\uFF1A 1."
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
