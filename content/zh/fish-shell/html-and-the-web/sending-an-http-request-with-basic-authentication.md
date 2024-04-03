---
date: 2024-01-20 18:01:27.623147-07:00
description: "How to: (\u5982\u4F55\u505A\uFF1A) \u5728Fish Shell\u4E2D\u53D1\u9001\
  \u5E26\u6709\u57FA\u672C\u8BA4\u8BC1\u7684HTTP\u8BF7\u6C42\u76F8\u5F53\u76F4\u63A5\
  \u3002\u4E0B\u9762\u7684\u4F8B\u5B50\u5C55\u793A\u4E86\u5982\u4F55\u4F7F\u7528`curl`\u547D\
  \u4EE4\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:48.265309-06:00'
model: gpt-4-1106-preview
summary: "\u5728Fish Shell\u4E2D\u53D1\u9001\u5E26\u6709\u57FA\u672C\u8BA4\u8BC1\u7684\
  HTTP\u8BF7\u6C42\u76F8\u5F53\u76F4\u63A5\u3002\u4E0B\u9762\u7684\u4F8B\u5B50\u5C55\
  \u793A\u4E86\u5982\u4F55\u4F7F\u7528`curl`\u547D\u4EE4."
title: "\u4F7F\u7528\u57FA\u672C\u8BA4\u8BC1\u53D1\u9001 HTTP \u8BF7\u6C42"
weight: 45
---

## How to: (如何做：)
在Fish Shell中发送带有基本认证的HTTP请求相当直接。下面的例子展示了如何使用`curl`命令。

```Fish Shell
set user "your_username"
set pass "your_password"

echo -n "$user:$pass" | base64 | read -l encoded_credentials

curl -H "Authorization: Basic $encoded_credentials" "http://example.com/resource"
```

假设请求成功，你可能会看到类似这样的输出:

```Fish Shell
HTTP/1.1 200 OK
...
```

如果认证失败，你会得到:

```Fish Shell
HTTP/1.1 401 Unauthorized
...
```

## Deep Dive (深入了解)
发送HTTP请求最早可追溯到早期互联网，当时用于简单的文件和消息传输。基础认证是HTTP/1.0的一部分，后来在1996年随HTTP/1.1标准一起发布。

尽管有安全性的问题，基本认证因其简易性而被广泛使用，特别是在内部或低安全风险的场合。它会以明文传递用户和密码（尽管经过Base64编码），所以建议只在HTTPS上使用。

除了`curl`，还有多种工具和库可以发送基础认证请求，比如`wget`命令及编程语言中的HTTP客户端库，例如Python的`requests`。

## See Also (另请参阅)
- [Fish Shell 官网](https://fishshell.com/)
- [cURL 官方文档](https://curl.se/docs/)
- [HTTP/1.1 规范: 认证](https://tools.ietf.org/html/rfc7617)
