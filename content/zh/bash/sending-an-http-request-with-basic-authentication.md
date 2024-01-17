---
title:                "使用基本身份验证发送http请求"
html_title:           "Bash: 使用基本身份验证发送http请求"
simple_title:         "使用基本身份验证发送http请求"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/bash/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# Basic Authentication: 如何用Bash发送HTTP请求

## What & Why?
通过Bash发送带有基本认证的HTTP请求是指在代码中使用Bash命令来发送一个带有基本认证信息的HTTP请求。程序员这样做的原因是为了在请求中加入认证信息，以便访问需要登录的网页或API。

## How to:
下面是一个用Bash发送带有基本认证HTTP请求的示例代码：

 ```
curl -u username:password https://example.com/api/users
```
这个命令将向URL为"https://example.com/api/users"的API发送一个HTTP GET请求，并在其中包含通过认证的用户名和密码。如果认证成功，API将会返回请求的数据。

### Sample Output:
如果认证成功，API将会返回请求的数据，例如：

```
[
    {
        "id": 1,
        "username": "JohnDoe",
        "email": "john@example.com"
    },
    {
        "id": 2,
        "username": "JaneSmith",
        "email": "jane@example.com"
    }
]
```

## Deep Dive:
### Historical Context:
基本认证是HTTP协议中最早的认证方式之一，它的设计目的是为了解决HTTP请求中必须包含特定用户的凭据的问题。在早期的互联网应用中，它被广泛地使用。但是由于它的安全性较低，现在已被许多其他更安全的认证方式所取代。

### Alternatives:
目前有许多其他的认证方式可供选择，比如OAuth、JWT等。它们都提供了更强的安全性和其他功能，适合不同的使用场景。

### Implementation Details:
使用Bash发送带有基本认证HTTP请求的关键是要将认证信息包含在请求中的HTTP头部信息中，具体格式为：Authorization: Basic <base64 encoded username:password>。

## See Also:
- [Bash官方文档](https://www.gnu.org/software/bash/)
- [curl命令文档](https://www.man7.org/linux/man-pages/man1/curl.1.html)
- [HTTP 1.0 RFC Section 11.1](https://tools.ietf.org/html/rfc1945#section-11.1)
- [HTTP Basic Authentication](https://developer.mozilla.org/zh-CN/docs/Web/HTTP/Authentication#basic_authentication_scheme)