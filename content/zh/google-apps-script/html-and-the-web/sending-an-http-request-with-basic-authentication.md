---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:01:51.269326-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u5728Google Apps Script\u4E2D\uFF0C\u8981\
  \u53D1\u9001\u5E26\u6709\u57FA\u672C\u8BA4\u8BC1\u7684HTTP\u8BF7\u6C42\uFF0C\u4F60\
  \u9700\u8981\u4F7F\u7528`UrlFetchApp`\u670D\u52A1\u5E76\u914D\u5408\u4E00\u4E2A\
  base64\u7F16\u7801\u7684\u6388\u6743\u5934\u3002\u4EE5\u4E0B\u662F\u9010\u6B65\u6307\
  \u5357\uFF1A 1. **\u7F16\u7801\u51ED\u8BC1**\uFF1A\u9996\u5148\uFF0C\u5C06\u4F60\
  \u7684\u7528\u6237\u540D\u548C\u5BC6\u7801\u7528base64\u7F16\u7801\u3002Google Apps\u2026"
lastmod: '2024-04-05T22:38:46.383722-06:00'
model: gpt-4-0125-preview
summary: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u5728Google Apps Script\u4E2D\uFF0C\u8981\
  \u53D1\u9001\u5E26\u6709\u57FA\u672C\u8BA4\u8BC1\u7684HTTP\u8BF7\u6C42\uFF0C\u4F60\
  \u9700\u8981\u4F7F\u7528`UrlFetchApp`\u670D\u52A1\u5E76\u914D\u5408\u4E00\u4E2A\
  base64\u7F16\u7801\u7684\u6388\u6743\u5934\u3002\u4EE5\u4E0B\u662F\u9010\u6B65\u6307\
  \u5357\uFF1A 1. **\u7F16\u7801\u51ED\u8BC1**\uFF1A\u9996\u5148\uFF0C\u5C06\u4F60\
  \u7684\u7528\u6237\u540D\u548C\u5BC6\u7801\u7528base64\u7F16\u7801\u3002Google Apps\
  \ Script\u6CA1\u6709\u7528\u4E8E\u5B57\u7B26\u4E32\u7684native base64\u7F16\u7801\
  \u51FD\u6570\uFF0C\u56E0\u6B64\u4F60\u5C06\u4F7F\u7528Utilities.base64Encode\u6765\
  \u5B8C\u6210\u8FD9\u4E2A\u76EE\u7684\u3002"
title: "\u4F7F\u7528\u57FA\u672C\u8BA4\u8BC1\u53D1\u9001HTTP\u8BF7\u6C42"
weight: 45
---

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
