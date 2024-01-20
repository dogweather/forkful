---
title:                "使用基本认证发送http请求"
html_title:           "Bash: 使用基本认证发送http请求"
simple_title:         "使用基本认证发送http请求"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/fish-shell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 什么 & 为什么?

发送带有基本身份验证的HTTP请求是一种使用用户名和密码在网络请求的头部(`Header`)中，提供身份认证信息的方法。程序员这样做是为了与需要身份验证的服务进行通信，例如API等。

## 如何做:

Fish Shell 允许我们轻松地发送带基本身份验证的HTTP请求。以下是发送一个GET请求的示例，并在[https://httpbin.org/basic-auth/user/passwd](https://httpbin.org/basic-auth/user/passwd)上测试它的方法。

```fish
set url https://httpbin.org/basic-auth/user/passwd
set username user
set password passwd
set auth (echo -n $username:$password | base64)
curl -H "Authorization: Basic $auth" $url
```
运行以上代码后，您将看到如下所示的输出：

```shell
{
  "authenticated": true,
  "user": "user"
}
```

解码后的用户名和密码能确认您的登录身份。

## 深入探究

发送带有基本身份验证的HTTP请求是网络编程的一个重要部分。这种技术起源于1990年代早期，当时的互联网需要一种简单的方式来验证用户身份。但请注意，基本身份验证并不能提供充足的安全性，因为它以明文形式发送用户名和密码。因此，与基本身份验证相比，现代开发者更倾向于使用更安全的机制，如令牌基础的身份验证。

此外，身份验证方法的选择也与服务的实现有关。例如, RESTful服务常使用Token-based身份验证，而GraphQL可能会使用JWTs。了解服务的需求是选择合适的认证方法的关键。

## 参考资料

如果您希望进一步探索Fish Shell或HTTP基本身份验证，以下是一些可用的资源：

- [Fish Shell官方文档](https://fishshell.com/docs/current/index.html)
- [HTTP Basic Authentication](https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Authorization)
- [Curl命令和使用](https://curl.se/docs/manpage.html) 
- [HTTPBin用于测试HTTP请求](https://httpbin.org/)