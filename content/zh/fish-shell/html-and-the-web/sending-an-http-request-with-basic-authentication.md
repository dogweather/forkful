---
date: 2024-01-20 18:01:27.623147-07:00
description: "\u53D1\u9001\u5177\u6709\u57FA\u672C\u8BA4\u8BC1\u7684HTTP\u8BF7\u6C42\
  \u5C31\u662F\u5728\u8BBF\u95EE\u9700\u8981\u7528\u6237\u540D\u548C\u5BC6\u7801\u7684\
  \u7F51\u7EDC\u8D44\u6E90\u65F6\uFF0C\u5C06\u51ED\u8BC1\u5305\u542B\u5728\u8BF7\u6C42\
  \u4E2D\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\u901A\u5E38\u662F\u4E3A\u4E86\u4E0E\
  \u53D7\u4FDD\u62A4\u7684API\u6216\u670D\u52A1\u4EA4\u4E92\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:48.265309-06:00'
model: gpt-4-1106-preview
summary: "\u53D1\u9001\u5177\u6709\u57FA\u672C\u8BA4\u8BC1\u7684HTTP\u8BF7\u6C42\u5C31\
  \u662F\u5728\u8BBF\u95EE\u9700\u8981\u7528\u6237\u540D\u548C\u5BC6\u7801\u7684\u7F51\
  \u7EDC\u8D44\u6E90\u65F6\uFF0C\u5C06\u51ED\u8BC1\u5305\u542B\u5728\u8BF7\u6C42\u4E2D\
  \u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\u901A\u5E38\u662F\u4E3A\u4E86\u4E0E\u53D7\
  \u4FDD\u62A4\u7684API\u6216\u670D\u52A1\u4EA4\u4E92\u3002."
title: "\u4F7F\u7528\u57FA\u672C\u8BA4\u8BC1\u53D1\u9001 HTTP \u8BF7\u6C42"
weight: 45
---

## What & Why? (什么和为什么？)
发送具有基本认证的HTTP请求就是在访问需要用户名和密码的网络资源时，将凭证包含在请求中。程序员这样做通常是为了与受保护的API或服务交互。

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
