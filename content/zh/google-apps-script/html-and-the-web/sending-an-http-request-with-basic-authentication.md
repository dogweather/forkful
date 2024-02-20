---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:01:51.269326-07:00
description: "\u53D1\u9001\u5E26\u6709\u57FA\u672C\u8BA4\u8BC1\u7684HTTP\u8BF7\u6C42\
  \u6D89\u53CA\u5C06\u7528\u6237\u540D\u548C\u5BC6\u7801\u7F16\u7801\u5230\u8BF7\u6C42\
  \u5934\u4E2D\uFF0C\u4EE5\u8BBF\u95EE\u53D7\u4FDD\u62A4\u7684\u8D44\u6E90\u3002\u7A0B\
  \u5E8F\u5458\u4F7F\u7528\u8FD9\u79CD\u65B9\u6CD5\u8FDB\u884C\u670D\u52A1\u5668\u7AEF\
  \u8BA4\u8BC1\uFF0C\u4EE5\u96C6\u6210\u9700\u8981\u57FA\u672C\u8BA4\u8BC1\u7684API\uFF0C\
  \u7528\u4E8E\u64CD\u4F5C\u5982\u6570\u636E\u68C0\u7D22\u6216\u53D1\u5E03\u5185\u5BB9\
  \u7B49\u3002"
lastmod: 2024-02-19 22:05:06.277379
model: gpt-4-0125-preview
summary: "\u53D1\u9001\u5E26\u6709\u57FA\u672C\u8BA4\u8BC1\u7684HTTP\u8BF7\u6C42\u6D89\
  \u53CA\u5C06\u7528\u6237\u540D\u548C\u5BC6\u7801\u7F16\u7801\u5230\u8BF7\u6C42\u5934\
  \u4E2D\uFF0C\u4EE5\u8BBF\u95EE\u53D7\u4FDD\u62A4\u7684\u8D44\u6E90\u3002\u7A0B\u5E8F\
  \u5458\u4F7F\u7528\u8FD9\u79CD\u65B9\u6CD5\u8FDB\u884C\u670D\u52A1\u5668\u7AEF\u8BA4\
  \u8BC1\uFF0C\u4EE5\u96C6\u6210\u9700\u8981\u57FA\u672C\u8BA4\u8BC1\u7684API\uFF0C\
  \u7528\u4E8E\u64CD\u4F5C\u5982\u6570\u636E\u68C0\u7D22\u6216\u53D1\u5E03\u5185\u5BB9\
  \u7B49\u3002"
title: "\u4F7F\u7528\u57FA\u672C\u8BA4\u8BC1\u53D1\u9001HTTP\u8BF7\u6C42"
---

{{< edit_this_page >}}

## 什么以及为什么？

发送带有基本认证的HTTP请求涉及将用户名和密码编码到请求头中，以访问受保护的资源。程序员使用这种方法进行服务器端认证，以集成需要基本认证的API，用于操作如数据检索或发布内容等。

## 如何操作：

在Google Apps Script中，要发送带有基本认证的HTTP请求，你需要使用`UrlFetchApp`服务并配合一个base64编码的授权头。以下是逐步指南：

1. **编码凭证**：首先，将你的用户名和密码用base64编码。Google Apps Script没有用于字符串的native base64编码函数，因此你将使用Utilities.base64Encode来完成这个目的。

```javascript
var username = 'YourUsername';
var password = 'YourPassword';
var encodedCredentials = Utilities.base64Encode(username + ':' + password);
```

2. **设置请求选项**：准备好编码凭证后，为HTTP请求准备选项对象，包括方法和头部信息。

```javascript
var options = {
  method: 'get', // 或 'post', 'put', 根据你的需求
  headers: {
    'Authorization': 'Basic ' + encodedCredentials
  }
  // 可以在这里添加额外的选项，如 'muteHttpExceptions' 用于错误处理
};
```

3. **发出请求**：使用`UrlFetchApp.fetch`方法，结合目标URL和选项对象。

```javascript
var url = 'https://example.com/api/resource';
var response = UrlFetchApp.fetch(url, options);
Logger.log(response.getContentText());
```

成功请求的示例输出将根据API的响应而变化。对于基于JSON的API，你可能会看到类似的内容：

```
{"status":"Success","data":"Resource data here..."}
```

确保通过检查响应代码或使用`muteHttpExceptions`选项来处理可能的HTTP错误，以便更加控制错误管理。

## 深入了解

发送带有基本认证的HTTP请求在许多编程语言中一直是用于访问需要认证的基于网络的资源的标准方法。在Google Apps Script的上下文中，`UrlFetchApp`提供了执行这些HTTP请求的直接方法，包括那些需要认证的请求。请求头中包含基本凭据是一种简单却有效的方法，但由于凭据以纯文本形式（仅base64编码）发送，如果被拦截可以轻易解码，因此带来安全隐患。

为了增强安全性，尤其是在处理敏感数据或操作时，推荐使用如OAuth 2.0等替代方案。Google Apps Script对于支持此协议的服务，通过`OAuth2`库内置了对OAuth 2.0的支持，简化了认证过程。

尽管存在安全限制，基本认证仍广泛用于非暴露于更广泛互联网的简单或内部应用。它易于实施，因为它只需要一个设置了合适头部的单一请求，使其成为快速集成的有吸引力选项，或者在没有更高安全方法的API中。然而，程序员被敦促考虑安全影响，并在可用时探索更安全的替代方案。
