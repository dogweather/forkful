---
title:                "使用基本认证发送http请求"
html_title:           "Bash: 使用基本认证发送http请求"
simple_title:         "使用基本认证发送http请求"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/bash/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 是什么以及为何（What & Why?）

HTTP请求的基本认证：就是对HTTP请求添加用户名和密码生效的认证方式。为什么要这样做？很简单：程序员用这种方法来获取对服务的受保护的访问权限。

## 如何操作（How to:）

在Bash中发送带有基本认证的HTTP请求，你需要使用`curl`命令。你需要把用户名和密码与URL一起输入。

以下是基本代码的实际显示：

```Bash
$ curl -u username:password http://example.com
```

你可以用实际的用户名、密码和URL替代它。


## 深入探索（Deep Dive）

基本认证的概念可以追溯到HTTP/1.0。这是一种最基础的认证方法，容易理解，轻松实施。

虽然基本认证效果明显，但在当前互联网环境下，有更多的认证方式值得考虑。比如：摘要认证、OAuth，或JWT。每一种方法都有自己的优点和适用场景。

如果你想在Bash中使用这些高级认证方式，如OAuth，你可能需要安装和使用更专业的HTTP浏览器，比如`httpie`。

在Bash中，基本认证是通过在HTTP头部添加‘Authorization’字段及其值来实现的。这个字段的值是用Base64进行编码的'username:password'。


## 请参阅（See Also）

- Curl的官方文档：https://curl.se/docs/
- Httpie的官方文档：https://httpie.io/
- 关于OAuth的详细信息：https://oauth.net/