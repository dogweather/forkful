---
title:                "使用基本身份验证发送http请求"
html_title:           "Gleam: 使用基本身份验证发送http请求"
simple_title:         "使用基本身份验证发送http请求"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/gleam/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 为什么

当我们需要与一个需要身份验证的服务器进行通信时，我们通常会发送一个HTTP请求。使用基本身份验证可以帮助我们向服务器验证我们的身份，从而获得访问权限。 

## 如何操作

在使用Gleam语言中发送HTTP请求时，我们需要使用`gleam/http`模块，以及`gleam/base64`模块来编码我们的验证信息。首先，我们需要定义一个函数来生成包含用户名和密码的Base64编码串。在发送HTTP请求时，我们需要在请求头中添加`Authorization`字段，并使用`Basic`关键字和我们生成的Base64编码串来进行身份验证。下面是一个示例代码：

```gleam
import gleam/base64
import gleam/http

pub fn authorization_header(username, password) {
  let base64_auth = base64.encode(\`$\{username\}:\{$password\}\`)
  http.header("Authorization", "Basic " ++ base64_auth)
}
```

我们可以在发送HTTP请求时调用这个函数，并将返回结果添加到请求头中。下面是一个完整的发送HTTP请求的代码示例：

```gleam
import gleam/base64
import gleam/http

pub fn send_request(username, password, url) {
  let auth_header = authorization_header(username, password)
  let client = http.client()
  let request = http.get(url)
  let request = http.add_header(request, auth_header)

  case http.send(client, request) {
    Ok(response) -> 
      # 这里是处理返回结果的逻辑
      discord.upload("Response: " ++ response.body)

    Err(error) ->
      discord.upload("Error: " ++ error)
  }
}
```

在上面的代码示例中，我们使用基本身份验证来发送HTTP请求，这样可以保证我们的请求正确被服务器接收并处理。 

## 深入了解

基本身份验证是一种简单而广泛应用的身份验证方法，它通过在每个HTTP请求中添加验证头来实现。在发送HTTP请求时，我们需要特别注意如何安全地存储和使用用户名和密码，以免被攻击者利用。另外，我们也可以结合使用HTTPS来提高数据传输的安全性。 

## 参考链接

- [Gleam官方文档](https://gleam.run/)
- [HTTP模块文档](https://gleam.run/modules/base64.html)
- [Base64模块文档](https://gleam.run/modules/http.html)
- [参考文章：What is Basic Access Authentication?](https://www.digitalocean.com/community/tutorials/what-is-basic-access-authentication)