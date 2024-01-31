---
title:                "使用基本认证发送 HTTP 请求"
date:                  2024-01-20T18:01:27.623147-07:00
model:                 gpt-4-1106-preview
simple_title:         "使用基本认证发送 HTTP 请求"

category:             "Fish Shell"
tag:                  "HTML and the Web"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/fish-shell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

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
