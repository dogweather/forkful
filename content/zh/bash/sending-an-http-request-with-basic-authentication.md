---
title:                "使用基本认证发送 HTTP 请求"
date:                  2024-01-20T18:00:58.220493-07:00
model:                 gpt-4-1106-preview
simple_title:         "使用基本认证发送 HTTP 请求"

tag:                  "HTML and the Web"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/bash/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？
发送带有基本认证的HTTP请求意味着在请求中附带用户名和密码。程序员这样做是为了与需要身份验证的服务器通信，访问受保护的资源。

## 如何操作：
使用 `curl` 命令发送带有基本认证的HTTP请求：

```Bash
# 使用用户名:user 密码:password 发送请求
curl -u user:password http://example.com

# 如果需要更详细的服务器响应信息，可以添加 -v 参数
curl -v -u user:password http://example.com
```

示例输出：

```Bash
# 假设服务器响应的状态码是200，这表示请求成功
HTTP/1.1 200 OK
Date: Mon, 23 Mar 2023 12:35:15 GMT
Server: Apache/2.4.41 (Ubuntu)
...
```

## 深度解析：
发送带基本认证的HTTP请求在早期Web开发中非常常见，即使现在也广泛使用。它的历史根源于HTTP/1.0协议。尽管有更安全的认证方式，比如OAuth和JWT，基本认证因其简单和广泛支持仍被频繁使用。实现它通常意味着用base64编码“用户名:密码”，但这种编码不是加密，易于解码，所以不应在不安全的网络中使用。

## 参看其他：
- HTTP认证官方文档: https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication
- `curl` 命令详细介绍: https://curl.se/docs/manpage.html
- 安全的HTTP认证方法: https://owasp.org/www-community/controls/Authentication
