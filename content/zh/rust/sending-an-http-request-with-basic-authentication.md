---
title:                "使用基本认证发送http请求"
html_title:           "Bash: 使用基本认证发送http请求"
simple_title:         "使用基本认证发送http请求"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/rust/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 什么与为何？

HTTP基本身份认证请求是一种利用用户名和密码为HTTP请求提供授权的方式。程序员常常会使用此方式来处理那些需要验证用户身份的敏感请求。

## 如何使用：

使用Rust发送一个带有基本认证的HTTP请求并不复杂，我们可以利用 reqwest 这个库，这就是相关的代步：

```Rust
use reqwest::blocking::Client;

fn main() {
   let client = Client::new();
   let res = client.get("http://httpbin.org/basic-auth/user/passwd")
      .basic_auth("user", Some("passwd"))
      .send();
   match res {
      Ok(_) => println!("成功获取请求！"),
      Err(_) => println!("出现了错误。"),
   };
}
```

在这个代码中，我们使用 `.basic_auth("user", Some("passwd"))` 设置了基本认证的用户名及密码。函数的执行结果会通过 Rust 的 `match` 结构体进行处理。如果请求成功，将会打印 "成功获取请求！"，若请求失败，则打印 "出现了错误。"

## 深入剖析：

历史背景中，HTTP基本认证是 HTTP/1.0 标准中定义的一部分，用于提供用户与密码的验证方式。然而，这种方式并不安全，因为传输中的数据没有经过加密，并且服务器端将密码存入明文，因此现在已被更为安全的方式所替代。

尽管如此，基本认证仍被广泛用于各种需要验证用户身份的场合，比如API测试等。不过在生产环境中使用时，必须经过SSL/TLS 的安全连接。

Rust的reqwest库为HTTP基本认证提供了简单明了的实现。有别于直接操作headers，reqwest可以透明地处理base64编码及必要的头部信息。

## 参考资料：

- Rust reqwest 库的官方文档： [https://docs.rs/reqwest](https://docs.rs/reqwest)
- HTTP基本认证的定义和概念： [https://zh.wikipedia.org/wiki/HTTP%E5%9F%BA%E6%9C%AC%E8%AE%A4%E8%AF%81](https://zh.wikipedia.org/wiki/HTTP%E5%9F%BA%E6%9C%AC%E8%AE%A4%E8%AF%81)
- 更多有关HTTP认证的细节： [https://developer.mozilla.org/zh-CN/docs/Web/HTTP/Authentication](https://developer.mozilla.org/zh-CN/docs/Web/HTTP/Authentication)